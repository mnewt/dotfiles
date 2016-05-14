function update -d 'Update software on the computer'

  # OS Specific
  switch (uname)
    case Linux
      if test -e /etc/arch-release
        sudo /usr/bin/pacman -Syu --noconfirm
        if sudo /usr/bin/pacman -Qtdq > /dev/null
          sudo /usr/bin/pacman -Rns (/usr/bin/pacman -Qtdq | sed -e ':a;N;\$!ba;s/\n/ /g')
        end
        sudo paccache -r
        sudo paccache -ruk0
        sudo pacman-optimize
      else if test -e /etc/debian_version; or test -e /etc/lsb_release
        sudo apt update
        sudo apt upgrade -y
        sudo apt-get autoremove -y
        sudo apt-get autoclean -y
      end
    case Darwin
      sudo softwareupdate -i -a
      if installed brew
        brew update; and brew upgrade --all; and brew cleanup; and brew prune; and brew doctor
        # update brew casks -- eventually this should not be necessary
        # (https://github.com/caskroom/homebrew-cask/issues/4678)
        for c in (brew cask list)
          if brew cask info "$c" | grep -qF "Not installed"
            brew cask uninstall "$c"
            rm -rf "/opt/homebrew-cask/Caskroom/$c"
            brew cask install "$c"
          end
        end
        brew cask cleanup
      end
  end

  # node.js
  if installed npm
    npm install npm -g
    npm update -g
  end

  # ruby
  if installed gem
    sudo gem update --system
    sudo gem update (gem list | cut -d ' ' -f 1)
    sudo gem clean
  end

  # python
  if installed pip
    pip install --upgrade pip
    pip freeze --local | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U
  end

  #iTerm2
  curl -L "https://iterm2.com/misc/"(basename $SHELL)"_startup.in" >>"$HOME/.iterm2/iterm2_shell_integration.fish"

end
