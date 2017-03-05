function source_if -d "If file exists, then source it"
  if test -e "$argv[1]"
    source "$argv[1]"
  end
end
