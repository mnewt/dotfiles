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
# alias G='| grep'
# alias M='| less'
# alias L='| wc -l'
# alias ONE="| awk '{ print \$1}'"

# Enable aliases to be sudo’ed
alias sudo='sudo '

# ls
# add formatting and color to `ls`
# Detect which `ls` flavor is in use
set -x CLICOLOR=1
set -xU LSCOLORS=ExFxCxDxBxegedabagacad
set -xU LS_COLORS "di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34:su=0:sg=0:tw=0:ow=0:"
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
set -xU EDITOR=vim
# use vim as pager / less replacement
if hash vimpager >/dev/null 2>&1
  set -xU PAGER (which vimpager)
  alias less $PAGER
end

# git
alias g="git"
# tell git to non-interactively merge commits
set -x GIT_MERGE_AUTOEDIT no
function gpl
  git log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit
end

# Network
alias whats-my-ip="dig +short myip.opendns.com @208.67.222.222 @208.67.220.220"
alias dis='dig +nocmd +noall +answer'
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

# OS Specific ##############################################

switch (uname)
  case Linux
    alias listening-ports='netstat -ntlp | grep LISTEN'

  case CYGWIN'*'
    alias listening-ports='netstat -an | grep LISTEN'
    alias ips ipconfig | awk -F" ." '/Address/ {print $NF}'
    # Flush Directory Service cache
    alias flush="ipconfig /flushdns"

  case Darwin
    # Set where to install casks
    set -x HOMEBREW_CASK_OPTS "--appdir=/Applications"

    # GNU coreutils: replace mac builtins
    if test -d /usr/local/opt/coreutils/libexec/gnubin
      set -x PATH /usr/local/opt/coreutils/libexec/gnubin $HOME/.bin /usr/local/bin /usr/bin /bin /usr/sbin /sbin
      set -gx MANPATH :/usr/local/opt/coreutils/libexec/gnuman
    end

    alias listening-ports='lsof -i -n -P | grep LISTEN'

    # open man page in Preview
    function pman
      man -t $argv[1] | open -f -a /Appllication/Preview.app/
    end

    # Change Directory to the active Finder window (else ~/Desktop)
    function cdf
      local fPath=`osascript -e '
      tell app "finder"
         try
            set folderPath to (folder of the front window as alias)
         on error
            set folderPath to (path to desktop folder as alias)
         end try
         POSIX path of folderPath
      end tell'
      `;
      echo "cd $fPath";
      cd "$fPath" > /dev/null
    end

    # Dash
    function dash 
      open "dash://"$argv[1]
    end

    # Get OS X Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
    function update-mac
      sudo softwareupdate -i -a
      brew update; brew upgrade; brew cleanup;
      npm install npm -g; npm update -g;
      #sudo gem update --system; sudo gem update
    end

    # Flush Directory Service cache
    function flush
      dscacheutil -flushcache
      killall -HUP mDNSResponder
    end

    # Recursively delete `.DS_Store` files
    alias clean="find . -type f -name '*.DS_Store' -ls -delete"

    # Empty the Trash on all mounted volumes and the main HDD
    # Also, clear Apple’s System Logs to improve shell startup speed
    alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

    # Show/hide hidden files in Finder
    alias show="defaults write com.apple.finder AppleShowAllFiles -bool true; and killall Finder"
    alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false; and killall Finder"

    # Merge PDF files
    # Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
    alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

    # Disable Spotlight
    alias spotoff="sudo mdutil -a -i off"
    # Enable Spotlight
    alias spoton="sudo mdutil -a -i on"
end


# GRC ######################################################
set -l GRC (which grc)
if test -e $GRC
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
    alias st="nohup /usr/bin/sublime_text $argv > /dev/null &"
  case CYGWIN'*'
    alias st=(which subl) $argv
  case Darwin
    alias st="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl "$argv
end
