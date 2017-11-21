require 'fileutils'

class Atom < Pack
  def self.exists?
    installed? 'apm'
  end

  def self.list
    puts installed_packages
  end

  def self.installed_packages
    @installed_packages ||= `apm list --bare --installed --dev false`.split(/[\s\n]/).map do |line|
      line.match(/^[^@]+/).to_s
    end
  end

  def self.configured_packages
    @configured_packages ||= section("apm")
  end

  def self.before
    @before_dir = FileUtils.pwd
    FileUtils.cd(ENV["TMPDIR"] || "/tmp")
  end
  
  def self.remove
    puts "Removing packages..."
    remove_packages = installed_packages - configured_packages
    puts remove_packages
    remove_packages.empty? or system "apm uninstall " + remove_packages.join(" ")
  end

  def self.upgrade
    puts "Upgrading packages..."
    system "apm upgrade --no-confirm"
  end

  def self.install
    puts "Installing packages..."
    add_packages = configured_packages - installed_packages
    puts add_packages
    add_packages.empty? or system "apm install " + add_packages.join(" ")
  end

  def self.after
    puts "Performing cleanup..."
    system "apm clean"
    FileUtils.cd(@before_dir)
  end
  
  def self.sync
    before
    remove
    upgrade
    install
    after
  end
end
