function fc --description 'Search through file contents with fzf'
  if [ "$argv[1]" != "" ]
    set p "$argv[1]"
  else
    set p *
  end
  grep --line-buffered --color=never -r "" $p | fzf
end
