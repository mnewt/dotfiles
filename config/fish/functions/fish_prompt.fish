# name: mnewt
# inspired by: Numist, https://gist.github.com/douglascamata/e409fd77e9c61dcbdc44
# and: https://gist.github.com/gak/5747159
#
# requires: fish, ncurses
#
# to benchmark, use the command:
# time -p fish -c 'fish_prompt'

# Just calculate these once, to save a few cycles when displaying the prompt
if not set -q __fish_prompt_hostname
  set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
end

if not set -q cyan
  # Colours
  set -g black (set_color black)
  set -g red (set_color red)
  set -g green (set_color green)
  set -g brown (set_color brown)
  set -g yellow (set_color yellow)
  set -g blue (set_color blue)
  set -g magenta (set_color magenta)
  set -g purple (set_color purple)
  set -g cyan (set_color cyan)
  set -g white (set_color white)
  set -g normal (set_color normal)
  # c0 to c4 progress from dark to bright
  # ce is the error colour
  set -g c0 (set_color 005284)
  set -g c1 (set_color 0075cd)
  set -g c2 (set_color 009eff)
  set -g c3 (set_color 6dc7ff)
  set -g c4 (set_color ffffff)
  set -g ce (set_color $fish_color_error)
end

if not set -q __fish_git_prompt_showstashstate
  set -g __fish_git_prompt_showstashstate 1
  set -g __fish_git_prompt_showuntrackedfiles 1
  set -g __fish_git_prompt_showdirtystate 1
  set -g __fish_git_prompt_showupstream 'auto'
  set -g __fish_git_prompt_showcolorhints 1
  # set -g __fish_git_prompt_color_branch (set_color purple)
end

if not set -q OSTYPE
  switch (uname)
    case CYGWIN'*'
      set OSTYPE 'CYGWIN'
    case 'Darwin'
      set OSTYPE 'Darwin'
    case 'Linux'
      set OSTYPE 'Linux'
  end
end


# Outputs first argument left-aligned, second argument right-aligned, newline
# function _rprint
#   if [ (count $argv) = 1 ]
#     echo -s $argv
#   else
#     set -l arglength (expr length + $argv[1]$argv[2])
#     set -l termwidth (tput cols)
#     set -l padding " "
#     if [ $arglength -lt $termwidth ]
#       set padding (printf "%"(expr $termwidth - $arglength)"s")
#     end

#     echo -n "$argv[1]$padding$argv[2]"
#   end
# end

function fish_prompt --description "Write out the prompt"
  # Last command
  set -l last_status $status

  # If in ssh session, print username and hostname
  if test -n "$SSH_CLIENT"
    echo -ns $yellow$USER$normal " at " $green$__fish_prompt_hostname$normal " in "
  end

  # Current Directory
  if test "$OSTYPE" = CYGWIN
    # shorten the path for CYGWIN, since we want the whole prompt on one line
    echo -ns $c1(prompt_pwd | sed "s,/,$c0/$c1,g" | sed "s,\(.*\)/[^m]*m,\1/$c3,")
  else
    # 1st sed replaces home dir with '~'
    # 2nd sed colorizes forward slashes
    # 3rd sed colorizes the deepest path (the 'm' is the last char in the
    # ANSI color code that needs to be stripped)
    echo -ns $c1(pwd | sed "s:^$HOME:~:" | sed "s,/,$c0/$c1,g" | sed "s,\(.*\)/[^m]*m,\1/$c3,")
  end

  # Git
  echo -ns (__fish_git_prompt "$normal on $purple%s")

  # Ruby
  # set -l ruby_info
  # if which rvm-prompt >/dev/null ^&1
  #   set ruby_info (rvm-prompt i v g)
  # else
  #   if which rbenv >/dev/null ^&1
  #     set ruby_info (rbenv version-name)
  #   end
  # end
  # test $ruby_info; and set ruby_info "$normal""using $magenta‹$ruby_info›"


  # The Cygwin/mintty/fish combination doesn't handle multi-line prompts well
  if test "$OSTYPE" != 'CYGWIN'
    echo
  end

    # Print last command status if nonzero
  set -e status_info
  if [ $last_status -ne 0 ]
    echo -ns "$ce$last_status"
  end

  # Prompt delimiter
  if [ (id -u $USER) = "0" ]
    echo -ns "$red# "
  else
    echo -ns "$normal> "
  end

end