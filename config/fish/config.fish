#!/usr/bin/env fish

set -e fish_greeting

function source_if -d "If file exists, then source it"
  if test -e "$argv[1]"
    source "$argv[1]"
  end
end

# source aliases and environment variables common to bash and fish
if functions -q bass
  if test -e "$HOME/.aliases"
    bass source "$HOME/.aliases"
  end
  if test -e "$HOME/.bin/start_ssh_agent"
    bass source "$HOME/.bin/start_ssh_agent"
  end
end

source_if "$HOME/.config/fish/functions.fish"
source_if "$HOME/.private.fish"

# iTerm2
if test "$TERM_PROGRAM" = "iTerm.app"
  source_if "$HOME/.bin/iterm2_shell_integration.fish"
end

if installed direnv
  eval (direnv hook fish)
end
