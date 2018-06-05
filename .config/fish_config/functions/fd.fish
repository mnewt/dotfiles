function fd --description 'cd to selected directory'
  if [ "$argv[1]" != "" ]
    set p "$argv[1]"
  else
    set p "."
  end
  find $p -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf +m | read dir
  cd "$dir"; or exit 1
end
