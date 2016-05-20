function update-arch
  if test -e /etc/arch-release
    sudo /usr/bin/pacman -Syu --noconfirm
    if sudo /usr/bin/pacman -Qtdq > /dev/null
      sudo /usr/bin/pacman -Rns (/usr/bin/pacman -Qtdq | sed -e ':a;N;\$!ba;s/\n/ /g')
    end
    sudo paccache -r
    sudo paccache -ruk0
    sudo pacman-optimize
  end
end

function update-debian
  if begin; test -e /etc/debian_version; or test -e /etc/lsb_release; end
    sudo apt update
    sudo apt upgrade -y
    sudo apt-get autoremove -y
    sudo apt-get autoclean -y
  end
end
function read_confirm -d 'Prompt user for confirmation'
  while true
    read -p "echo $argv" -l confirm
    switch $confirm
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end

function update-appstore
  sudo softwareupdate -i -a
end

function read_confirm
  while true
    read -p "echo $argv" -l confirm
    switch $confirm
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end

function update-homebrew
  if installed brew
    # update brew casks -- eventually this should not be necessary
    # (https://github.com/caskroom/homebrew-cask/issues/4678)
    echo "Updating homebrew casks..."
    for c in (brew cask list)
      if brew cask info "$c" | grep -q "Not installed"
        brew cask uninstall "$c"
        rm -rf "/opt/homebrew-cask/Caskroom/$c"
        brew cask install "$c"
      end
      if begin
            test "$argv[1]" = "-a"
            and brew cask info "$c" | grep -q ": latest"
            and read_confirm "Update non-versioned cask: $c\? [y/N]:"
          end
        echo "Reinstalling cask: $c..."
        brew cask uninstall "$c"
        rm -rf "/opt/homebrew-cask/Caskroom/$c"
        brew cask install "$c"
      end
    end
    brew cask cleanup
  end
end

function update-npm
  if installed npm
    npm install npm -g
    npm update -g
  end
end

function update-gems
  if installed gem
    sudo gem update --system
    sudo gem update (gem list | cut -d ' ' -f 1)
    sudo gem clean
  end
end

function update-pip
  if installed pip
    pip install --upgrade pip
    pip freeze --local | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U
  end
end

function update -d 'Update software on the computer'
  # OS Specific
  switch (uname)
    case Linux
      update-arch $argv
      update-debian $argv
    case Darwin
      update-appstore $argv
      update-homebrew $argv
      #iTerm2
      curl -L "https://iterm2.com/misc/"(basename $SHELL)"_startup.in" >>"$HOME/.iterm2/iterm2_shell_integration.fish"
  end
  update-npm
  update-gems
  update-pip
end
