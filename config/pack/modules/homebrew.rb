require 'fileutils'

class Homebrew < Pack
  def self.exists?
    system "command -v 'brew' >/dev/null 2>&1"
  end
  
  def self.list
    puts "==> taps:"
    puts installed_taps
    puts "==> recipes:" 
    puts installed_recipes
    puts "==> casks:" 
    puts installed_casks
  end

  def self.upgrade_cask(cask)
    puts ">> Upgrading cask: #{cask}..."
    system "brew cask uninstall --force \"#{cask}\""
    FileUtils.rm_rf(["/opt/homebrew-cask/Caskroom/#{cask}"])
    system "brew cask install \"#{cask}\"`"
  end

  # update brew casks -- eventually this should not be necessary
  # (https://github.com/caskroom/homebrew-cask/issues/4678)
  def self.upgrade_casks
    `brew cask outdated`.split("\n") do |cask|
      puts "upgrade_cask #{cask}"
    end
    # puts "| cask | installed version | current version |"
    # `brew cask list -1`.split(/[\s\n]/).each do |cask|
    #   info = `brew cask info #{cask}`
    #   current_version = info.split(/[\s]/)[1]
    #   installed_version = info.split(/[\s\n]/)[2].split(/[\s\/]/)[5]
    #   puts "| #{cask} | #{installed_version} | #{current_version} |"
    #   installed_version != current_version and upgrade_cask(cask)
    # end
  end

  def self.installed_taps
    @installed_taps ||= `HOMEBREW_NO_AUTO_UPDATE=1 brew tap -1`.split(/[\s\n]/)
  end

  def self.configured_taps
    @configured_taps ||= section("taps")
  end

  def self.installed_recipes
    @installed_recipes ||= `brew leaves -1`.split(/[\s\n]/)
  end

  def self.configured_recipes
    @configured_recipes ||= section("recipes")
  end

  def self.installed_casks
    @installed_casks ||= `brew cask list -1`.split(/[\s\n]/)
  end

  def self.configured_casks
    @configured_casks ||= section("casks")
  end
  
  def self.remove
    puts "Removing taps..."
    remove_taps = installed_taps - configured_taps
    puts remove_taps
    remove_taps.empty? or system "brew untap " + remove_taps.join(" ")
    
    puts "Removing recipes..."
    remove_recipes = installed_recipes - configured_recipes
    puts remove_recipes
    remove_recipes.empty? or system "brew uninstall " + remove_recipes.join(" ")

    puts "Removing casks..."
    remove_casks = installed_casks - configured_casks
    puts remove_casks
    remove_casks.empty? or system "brew cask uninstall " + remove_casks.join(" ")
  end

  def self.upgrade
    puts "Upgrading recipes..."
    system "brew upgrade"
    puts "Upgrading casks..."
    upgrade_casks
  end

  def self.install
    puts "Installing taps..."
    add_taps = configured_taps - installed_taps
    puts add_taps
    add_taps.empty? or system "brew tap " + add_taps.join(" ")

    puts "Installing recipes..."
    add_recipes = configured_recipes - installed_recipes
    puts add_recipes
    add_recipes.empty? or system "brew install " + add_recipes.join(" ")

    puts "Installing casks..."
    add_casks = configured_casks - installed_casks
    puts add_casks
    add_casks.empty? or system "brew cask install " + add_casks.join(" ")
  end

  def self.after
    brew_repo = `brew --repo`.chop
    puts "Performing housekeeping on brew repo: #{brew_repo}"
    system "git -C \"#{brew_repo}\" prune
            git -C \"#{brew_repo}\" gc
            brew cleanup
            brew prune
            brew cask cleanup
            brew doctor"
  end
  
  def self.sync
    remove
    upgrade
    install
    after
  end
end
