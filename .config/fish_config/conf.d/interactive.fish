# Initialization for interactive shells

if status --is-interactive
  # source aliases and environment variables common to {ba,fi,z}sh
  fix_if "$HOME/.env"
  fix_if "$HOME/.aliases"

  # Help with Emacs muscle memory
  bind \ep history-search-backward
  bind \en history-search-forward

  source_if "$HOME/.private.fish"
end
