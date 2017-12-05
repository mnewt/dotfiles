# Initialization for interactive shells

if status --is-interactive
  # source aliases and environment variables common to {ba,fi,z}sh
  fix_if "$HOME/.aliases"
  fix_if "$HOME/.bin/start-ssh-agent"

  # iTerm2
  if test "$TERM_PROGRAM" = "iTerm.app"
    source_if "$HOME/.bin/iterm2_shell_integration.fish"
  end

  # fzf
  set -gx FZF_LEGACY_KEYBINDINGS 0

  fix_if "$HOME/.private.sh"
  source_if "$HOME/.private.fish"
end
