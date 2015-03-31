#!/usr/local/bin/fish

# misc
alias tlf='tail -f'
alias ln='ln -v'
alias mkdir='mkdir -p'
alias ...='../..'
alias df='df -h'

# grep
set -x GREP_COLOR '3;33'
alias grep='grep --color=auto'

# sudo
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
function p
  ps u (pgrep $argv) | grep --color -E "$argv|\$"
end

# ls
# add formatting and color to `ls`
set -x CLICOLOR 1
set -xU LSCOLORS ExFxCxDxBxegedabagacad
set -xU LS_COLORS "di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34:su=0:sg=0:tw=0:ow=0:"
# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1 # GNU `ls`
  set -x colorflag "--color"
else # OS X `ls`
  set -x colorflag "-G"
end
alias ls="command ls -Fh "$colorflag
alias l='ls'
alias lh='ls -Alh'
alias ll='ls -Fhl'
alias lsd="ls -lF "$colorflag" | grep --color=never '^d'"

# vim
alias vi='vim'
# use vim as editor
set -xU EDITOR vim
# use vim as pager / less replacement
if which vimpager >/dev/null 2>&1
  set -xU PAGER (which vimpager)
  alias less $PAGER
end

# git
alias g="git"
alias gs="git status"
alias ga="git add ."
alias gc="git commit"
# tell git to non-interactively merge commits
set -x GIT_MERGE_AUTOEDIT no
function gpl
  git log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
end

# Network
# some networks block dns requests
#alias whats-my-ip="dig +short myip.opendns.com @208.67.222.222 @208.67.220.220"
alias whats-my-ip 'curl -s https://diagnostic.opendns.com/myip'
alias dis='dig +nocmd +noall +answer'

# csv
function prettycsv
  python -c 'import sys,csv; c = csv.reader(open(sys.stdin, "rU"), dialect=csv.excel_tab); [sys.stdout.write("^M".join(map(repr,r))+"\n") for r in c];' <"$argv" | column -s '^M' -t
end

# Recursively delete OS cache files
# COULD BE DANGEROUS
function clean-os-junk
  set -l files_to_delete '*.DS_Store' 'desktop.ini'
  if set -q argv
    set dir '.'
  else
    set dir $argv
  end
  for file in $files_to_delete
    find "$dir" -name "$file" -print
  end
  while true
    read -l -p 'echo "Do you wish to delete these files? [yN] "' yn
    set yn (echo $yn | tr '[:upper:]' '[:lower:]')
    switch $yn
      case 'y*'
        for file in $files_to_delete
          find "$dir" -name "$file" -print -delete
        end
        return 0
      case '*'
        echo 'Exiting, no files modified'
        return 0
    end
  end
end


# GRC ######################################################
set -l GRC (which grc >/dev/null 2>&1)
if not set -q $GRC
  alias colourify=$GRC" -es --colour=auto"
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
  alias curl='colourify -c conf.curl curl'
end

# Sublime Text #############################################

switch (uname)
  case Linux
    function st
      nohup /usr/bin/sublime_text $argv > /dev/null &
    end
  case CYGWIN'*'
    alias st='/cygdrive/c/Program Files/Sublime Text 3/Subl.exe'
  case Darwin
    alias st='/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl'
end

# Reload the shell (i.e. invoke as a login shell)
# set SHELL (which fish)
# alias reload="exec $SHELL -l"

# OS Specific ##############################################

switch (uname)
  case Linux
    alias listening-ports='netstat -lnptu | grep LISTEN'

    function ips
      /sbin/ifconfig |grep -B1 "inet" |awk '{ if ( $1 == "inet" ) { print $2 } else if ( $2 == "Link" ) { printf "%s:" ,$1 } }' |awk -F: '{ print $1 ": " $3 }'
    end

    function update
      sudo aptitude update
      sudo aptitude upgrade -y
      sudo aptitude clean
    end

  case 'CYGWIN*'
    alias listening-ports='netstat -an | grep LISTENING'
    alias ips ipconfig | awk -F" ." '/Address/ {print $NF}'
    # Flush Directory Service cache
    alias flush="ipconfig /flushdns"

  case Darwin
    # Set where to install casks
    set -x HOMEBREW_CASK_OPTS "--appdir=/Applications"

    alias listening-ports='lsof -i -n -P | grep LISTEN'

    #alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"
    function ips
      # fish only recognizes a list coming from std when it's delimited by '\n'
      # even though it displays lists as delimited by ' '
      # https://github.com/fish-shell/fish-shell/issues/156
      for i in (ifconfig -l | tr ' ' '\n')
        set -l ipaddr (ifconfig $i | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)')
        # this test is always true
        if test -n "$ipaddr"
          echo $i ":" $ipaddr
        end
      end
    end

    # open man page in Preview
    function pman
      man -t $argv[1] | open -f -a /Application/Preview.app/
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
      open "dash://"$argv[1]
    end


    # vagrant 
    set -x VAGRANT_DEFAULT_PROVIDER parallels

    function update
      sudo softwareupdate -i -a
      brew update; brew upgrade; brew cleanup;
      npm install npm -g; npm update -g;
      #sudo gem update --system; sudo gem update
    end

    # Flush Directory Service cache
    function flush
      sudo discoveryutil mdnsflushcache
      sudo discoveryutil udnsflushcache
    end

    # Empty the Trash on all mounted volumes and the main HDD
    # Also, clear Apple’s System Logs to improve shell startup speed
    alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

    # Show/hide hidden files in Finder
    alias show-hidden-files="defaults write com.apple.finder AppleShowAllFiles -bool true; and killall Finder"
    alias hide-hidden-files="defaults write com.apple.finder AppleShowAllFiles -bool false; and killall Finder"

    # Merge PDF files
    # Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
    alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

    # Disable Spotlight
    alias spotoff="sudo mdutil -a -i off"
    # Enable Spotlight
    alias spoton="sudo mdutil -a -i on"

    function recent-items-dock
      defaults write com.apple.dock persistent-others -array-add '{ "tile-data" = { "list-type" = 1; }; "tile-type" = "recents-tile"; }'
      killall Dock
    end
end
