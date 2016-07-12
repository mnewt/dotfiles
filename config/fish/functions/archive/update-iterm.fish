function update-iterm -d 'Update iTerm2'
  if set -q ITERM_PROFILE
    curl -L "https://iterm2.com/misc/fish_startup.in" >"$HOME/.iterm2/iterm2_shell_integration.fish"
    curl -L "https://iterm2.com/misc/bash_startup.in" >"$HOME/.iterm2/iterm2_shell_integration.sh"
    chmod +x "$HOME/.iterm2/*sh"
  end
end
