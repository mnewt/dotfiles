function update-iterm -d 'Update iTerm2'
  if set -q ITERM_PROFILE
    # curl -L "https://iterm2.com/misc/"(basename $SHELL)"_startup.in" >"$HOME/.iterm2_shell_integration.fish"
    # chmod +x "$HOME/.iterm2_shell_integration.fish"

  end
end
