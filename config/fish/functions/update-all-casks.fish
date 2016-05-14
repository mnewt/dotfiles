function update -d 'Update all homebrew casks'

  # OS Specific
  switch (uname)
    case Darwin
      if installed brew
        # update brew casks -- eventually this should not be necessary
        # (https://github.com/caskroom/homebrew-cask/issues/4678)
        for c in (brew cask list)
          if brew cask info "$c" | grep -qE "Not installed|: latest"
            brew cask uninstall "$c"
            rm -rf "/opt/homebrew-cask/Caskroom/$c"
            brew cask install "$c"
          end
        end
        brew cask cleanup
      end
  end
