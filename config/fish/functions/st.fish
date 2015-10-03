function st -d 'Launch Sublime Text 3'
  # OS Specific
  switch (uname)
    case Linux
      nohup /usr/bin/sublime_text $argv > /dev/null &
    case Darwin
      /Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl $argv
    case 'CYGWIN*'
      alias st='/cygdrive/c/Program Files/Sublime Text 3/Subl.exe'
  end
end