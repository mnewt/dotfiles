require 'fileutils'

class Debian < Pack
  def self.exists?
    File.exist? '/etc/debian_version' or File.exist? '/etc/lsb-release'
  end

  def self.list
    puts installed.packages
  end

  def self.installed_packages
    @installed_packages ||= `apt list --installed`.split(/[\s\n]/).map do |line|
      line.match(/^[^\/]+/).to_s
    end
  end

  def self.configured_packages
    @configured_packages ||= section("apt")
  end

  def self.before
    system "sudo pacman-key --refresh-keys"
  end
  
  def self.remove
    puts "Removing packages..."
    remove_packages = installed_packages - configured_packages
    puts remove_packages
    remove_packages.empty? or system "sudo pacman -R " + remove_packages.join(" ")
  end

  def self.upgrade
    puts "Upgrading packages..."
    system "sudo pacman -Syu --noconfirm"
  end

  def self.install
    puts "Installing packages..."
    add_packages = configured_packages - installed_packages
    puts add_packages
    add_packages.empty? or system "sudo pacman -S " + add_packages.join(" ")
  end

  def self.after
    puts "Performing cleanup..."
    system "sudo pacman -Qtdq > /dev/null && \
            sudo pacman --noconfirm -Rns $(pacman -Qtdq)
            sudo paccache -r
            sudo paccache -ruk0
            sudo pacman-optimize"
  end
  
  def self.sync
    before
    remove
    upgrade
    install
    after
  end
end
