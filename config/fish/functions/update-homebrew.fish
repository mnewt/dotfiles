function update-homebrew -d 'Update homebrew packaages'
  if installed brew
    # update brew casks -- eventually this should not be necessary
    # (https://github.com/caskroom/homebrew-cask/issues/4678)
    echo "Updating homebrew..."
    brew update
    and brew upgrade --all
    and brew cleanup
    and brew prune
    and brew doctor
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
            and prompt_confirm "Update non-versioned cask: $c\?"
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
