#!/usr/bin/env fish

set -e fish_greeting

# Homebrew installs to /usr/local/sbin and /usr/local/opt/coreutils/libexec/gnubin
for p in "/usr/local/opt/coreutils/libexec/gnubin" "/usr/local/sbin" "$HOME/.bin"
  test -d "$p"; and set -gx PATH $p $PATH
end

function source_if -d "If file exists, then source it"
  if test -e "$argv[1]"
    source "$argv[1]"
  end
end

if status --is-interactive
  # iTerm2
  if test "$TERM_PROGRAM" = "iTerm.app"
    source_if "$HOME/.bin/iterm2_shell_integration.fish"
  end

  # source aliases and environment variables common to bash and fish
  if functions -q bass
    if test -e "$HOME/.aliases"
      bass source "$HOME/.aliases"
    end
    if test -e "$HOME/.bin/start-ssh-agent"
      bass source "$HOME/.bin/start-ssh-agent"
    end
  end

  source_if "$HOME/.config/fish/functions.fish"
  source_if "$HOME/.private.fish"

  if installed direnv
    eval (direnv hook fish)
  end

  if installed rbenv
    source (rbenv init -|psub)
  end
end

