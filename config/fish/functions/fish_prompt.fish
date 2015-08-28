# name: mnewt
# inspired by: Numist, https://gist.github.com/douglascamata/e409fd77e9c61dcbdc44
# and: https://gist.github.com/gak/5747159
#
# requires: fish, ncurses
#
# to do a half baked benchmark, use the command:
# time -p fish -c 'fish_prompt'

# Cygwin is special
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

# Just calculate these once, to save a few cycles when displaying the prompt
if not set -q __fish_prompt_hostname
  if test "$OSTYPE" = CYGWIN
    # I do not understand why this is necessary, but it is
    set -g __fish_prompt_hostname (/bin/hostname)
  else
    set -g __fish_prompt_hostname (hostname -s)
  end
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

  # non standard colors
  set -g cyan (set_color 42f8ca)
  set -g gray (set_color 707070)

  # cyan gradient
  set -g c1 (set_color 059b75)
  set -g c2 (set_color 07cc9a)
  set -g c3 (set_color 10f7bc)
  set -g c4 (set_color 41f8ca)
  set -g c5 (set_color 6ff0cf)
  set -g c6 (set_color adf6e4)

  set -g ce (set_color $fish_color_error)
end

# if not set -q __fish_git_prompt_showstashstate
#   set -g __fish_git_prompt_showstashstate 1
#   set -g __fish_git_prompt_showuntrackedfiles 1
#   set -g __fish_git_prompt_showdirtystate 1
#   set -g __fish_git_prompt_showupstream 'auto'
#   set -g __fish_git_prompt_showcolorhints 1
#   # set -g __fish_git_prompt_color_branch (set_color purple)
# end

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
    echo -n -s "$yellow$USER$gray at $green$__fish_prompt_hostname$gray in "
  end

  # Current Directory
  if test "$OSTYPE" = CYGWIN
    # shorten the path for CYGWIN, since we want the whole prompt on one line
    echo -n -s $c1(prompt_pwd | sed -e "s,/,$c1/$c3,g" -e "s,\(.*\)/[^m]*m,\1/$c4,")
  else
    # 1st expression replaces home dir with '~'
    # 2nd expression colorizes forward slashes
    # 3rd expression colorizes the deepest path (the 'm' is the last char in the
    # ANSI color code that needs to be stripped)
    echo -n -s $c1(pwd | sed -e "s:^$HOME:~:" -e "s,/,$c1/$c3,g" -e "s,\(.*\)/[^m]*m,\1/$c4,")
  end

  # Git

  # This method is nice, easy, and detailed but slow (especially over SMB/NFS)
  #echo -n -s (__fish_git_prompt "$gray on $purple%s")

  # bare bones, much faster git info
  # check if we're in a git repo
  if git rev-parse --is-inside-work-tree ^/dev/null >/dev/null
    # branch name
    set -l git_branch (git symbolic-ref HEAD ^/dev/null | sed 's|^refs/heads/||')
    echo -n -s "$gray git $purple$git_branch"
    set -l dirty (git status --porcelain --ignore-submodules ^/dev/null)
    if test -n "$dirty"
      # repo is dirty
      echo -n -s (set_color -o purple) '*'
    end
  end

  # Vagrant
  # this doesn't work and it's too slow
  # this could be inspiration: (https://github.com/n00bworks/vagrant-status)
  # if test -e 'Vagrantfile'
  #   set -l vagrant_status (vagrant status 2>&1)
  #   if echo $vagrant_status | grep 'poweroff'
  #     echo -n -s " [off]"
  #   end
  #   if echo $vagrant_status | grep 'running'
  #     echo -n -s " [on]"
  #   end
  #   if echo $vagrant_status | grep 'aborted'
  #     echo -n -s " [aborted]"
  #   end
  #   if echo $vagrant_status | grep 'not created'
  #     echo -n -s " [not created]"
  #   end
  # end

  # Ruby
  # set -l ruby_info
  # if which rvm-prompt >/dev/null ^&1
  #   set ruby_info (rvm-prompt i v g)
  # else
  #   if which rbenv >/dev/null ^&1
  #     set ruby_info (rbenv version-name)
  #   end
  # end
  # test $ruby_info; and set ruby_info "$gray""using $magenta‹$ruby_info›"


  # Python / virtualenv / virtualfish

  if set -q VIRTUAL_ENV
    echo -n -s "$gray venv $blue" (basename $VIRTUAL_ENV)
  end


  # The Cygwin/mintty/fish combination doesn't handle multi-line prompts well
  if test "$OSTYPE" != 'CYGWIN'
    echo
  end

  # Print last command status if nonzero
  set -e status_info
  if test $last_status -ne 0
    echo -n -s "$gray" (set_color -b $fish_color_error) "$last_status" (set_color -b normal)
  end

  # Prompt delimiter
  if test "$USER" = "root"
    echo -n -s "$red# "
  else
    echo -n -s "$white> "
  end

  echo -n -s "$normal"

end