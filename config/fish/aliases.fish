#!/usr/bin/env fish

# misc
alias tf='tail -f'
alias ln='ln -v'
alias mkdir='mkdir -p'
alias ...='../..'
alias ....='../../..'
alias df='df -h'

# grep
set -x GREP_COLOR '3;33'
alias grep='grep --color=auto'

# sudo
# this doesn't work
# function sudo
#     if test "$argv" = !!
#         # bring back `sudo !!`
#         eval command sudo $history[1]
#     else
#         # allow aliases to be sudo'ed
#         command sudo $argv
#     end
# end

# ps
function p -d 'Search processes for $argv[1] and print the result'
  ps u (pgrep $argv) | grep --color -E "$argv|\$"
end
function sudop -d 'Search all processes for $argv[1] and print the result'
  sudo ps u (pgrep $argv) | grep --color -E "$argv|\$"
end

# ls
# add formatting and color to `ls`
set -x CLICOLOR 1
set -x LSCOLORS ExFxCxDxBxegedabagacad
set -x LS_COLORS "di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34:su=0:sg=0:tw=0:ow=0:"
# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1 # GNU `ls`
  set -x colorflag "--color"
else # OS X `ls`
  set -x colorflag "-G"
end
alias ls="command ls -Fh $colorflag"
alias l='ls'
alias lh='ls -Alh'
alias ll='ls -Fhl'
alias lsd="ls -lF $colorflag | grep --color=never '^d'"
alias lt='ls -hltr'

# vim
alias vi='vim'
# use vim as editor
set -x EDITOR (which vim)

# tmux
# TODO - connect to session or create new
alias t='tmux'


# PAGER
# use vimpager if available
if installed vimpager
  set -x PAGER (which vimpager)
  alias less $PAGER
  alias zless $PAGER
else
  if installed source-highlight
    set -x LESSOPEN "| /usr/local/bin/src-hilite-lesspipe.sh %s"
    set -x LESS " -R "
    alias less='less -m -g -i -J --underline-special --SILENT'
    alias more='less'
  end
end

# ssh
function sst
  ssh $argv -t "tmux -CC attach; or tmux -CC"
end

# git
alias g="git"
alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gcm="git commit -m"
alias gp="git push"
# tell git to non-interactively merge commits
set -x GIT_MERGE_AUTOEDIT no
function gpl
  git log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
end

# vagrant
alias v='vagrant'

# Network
# some networks block dns requests
#alias whats-my-ip="dig +short myip.opendns.com @208.67.222.222 @208.67.220.220"
alias whats-my-ip 'curl -s https://diagnostic.opendns.com/myip'
alias dis='dig +nocmd +noall +answer'

# csv
function prettycsv
  python -c 'import sys,csv; c = csv.reader(open(sys.stdin, "rU"), dialect=csv.excel_tab); [sys.stdout.write("^M".join(map(repr,r))+"\n") for r in c];' <"$argv" | column -s '^M' -t
end

# pw (https://gist.github.com/mnewt/8d2eef4150d93a90d273)
function pwcopy
  pw $argv | tee /dev/tty | pbcopy
end

# mutt
function mutt
    bash --login -c 'cd ~/Desktop; /usr/local/bin/mutt' $argv;
end


# GRC ######################################################
if installed grc
  alias colourify=(which grc)" -es --colour=auto"
  alias configure='colourify ./configure'
  alias diff='colourify diff'
  alias make='colourify make'
  alias gcc='colourify gcc'
  alias g++='colourify g++'
  alias as='colourify as'
  alias gas='colourify gas'
  alias ld='colourify ld'
  alias netstat='colourify netstat'
  alias ping='colourify ping'
  alias traceroute='colourify /usr/sbin/traceroute'
  alias arp='colourify -c conf.traceroute arp'
  alias tail='colourify -c conf.log tail'
  alias ps='colourify -c conf.ps ps'
  alias ifconfig='colourify -c conf.traceroute ifconfig'
  alias nmap='colourify -c conf.nmap nmap'
  alias lsof='colourify -c conf.traceroute lsof'
  alias dig='colourify -c conf.traceroute dig'
  alias host='colourify -c conf.traceroute host'
  alias drill='colourify -c conf.traceroute drill'
  alias curl='colourify -c conf.curl curl'
end


# Reload the shell (i.e. invoke as a login shell)
# set SHELL (which fish)
# alias reload="exec $SHELL -l"

# OS Specific ##############################################

switch (uname)
  case Linux

  case 'CYGWIN*'

  case Darwin
    # open man page in Preview
    function pman
      man -t $argv[1] | open -f -a /Applications/Preview.app/
    end

    # Quick look
    function ql
      qlmanage -p $argv 2>1 >/dev/null
    end

    # Change Directory to the active Finder window (else ~/Desktop)
    function cdf
      set -l fPath (osascript -e '
      tell app "finder"
         try
            set folderPath to (folder of the front window as alias)
         on error
            set folderPath to (path to desktop folder as alias)
         end try
         POSIX path of folderPath
      end tell'
      )
      echo "cd $fPath";
      cd "$fPath" > /dev/null
    end

    # Dash
    function dash
      open "dash://$argv"
    end

    # vagrant
    # set -x VAGRANT_DEFAULT_PROVIDER parallels

    # Empty the Trash on all mounted volumes and the main HDD
    # Also, clear Apple’s System Logs to improve shell startup speed
    alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

    # Show/hide hidden files in Finder
    alias show-hidden-files="defaults write com.apple.finder AppleShowAllFiles -bool true; and killall Finder"
    alias hide-hidden-files="defaults write com.apple.finder AppleShowAllFiles -bool false; and killall Finder"

    # Merge PDF files
    # Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
    # alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'
    function mergepdf
      /System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py $argv
    end

    # Disable Spotlight
    alias spotoff="sudo mdutil -a -i off"
    # Enable Spotlight
    alias spoton="sudo mdutil -a -i on"

    function recent-items-dock
      defaults write com.apple.dock persistent-others -array-add '{ "tile-data" = { "list-type" = 1; }; "tile-type" = "recents-tile"; }'
      killall Dock
    end

end
