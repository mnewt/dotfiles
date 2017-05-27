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
  # source aliases and environment variables common to bash and fish
  if functions -q fenv
    if test -e "$HOME/.aliases"
      fenv source "$HOME/.aliases"
    end
    if test -e "$HOME/.bin/start-ssh-agent"
      fenv source "$HOME/.bin/start-ssh-agent"
    end
  else
    echo "fenv is not installed -- you should probably run `fisher`"
  end

  # Set the syntax highlighting colors
  set_fish_colors

  # iTerm2
  if test "$TERM_PROGRAM" = "iTerm.app"
    source_if "$HOME/.bin/iterm2_shell_integration.fish"
  end

  source_if "$HOME/.private.fish"

  if installed direnv
    eval (direnv hook fish)
  end

  if installed rbenv
    source (rbenv init -|psub)
  end
end
