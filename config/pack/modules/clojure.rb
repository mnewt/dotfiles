require 'fileutils'

class Clojure < Pack
  def self.exists?
    installed? 'lein' or installed? 'boot'
  end

  def self.list
    puts "Not implemented."
  end

  def self.upgrade
    puts "Upgrading packages..."
    system "lein upgrade
            lein  ancient upgrade-profiles"
  end

  def self.sync
    upgrade
  end
end
