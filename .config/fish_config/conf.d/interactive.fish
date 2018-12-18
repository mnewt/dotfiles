# Initialization for interactive shells

if status --is-interactive
  # source aliases and environment variables common to {ba,fi,z}sh
  fix_if "$HOME/.env"
  fix_if "$HOME/.aliases"
  fix_if "$HOME/.bin/start-ssh-agent"

  # iTerm2
  if test "$TERM_PROGRAM" = "iTerm.app"
    source_if "$HOME/.bin/iterm2_shell_integration.fish"
  end

  # Help with Emacs muscle memory
  bind \ep history-search-backward
  bind \en history-search-forward

  source_if "$HOME/.private.fish"
end
