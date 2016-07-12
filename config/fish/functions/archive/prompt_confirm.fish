function prompt_confirm -d 'Prompt user for confirmation'
  set -l prompt "$argv [y/N]:"
  while true
    read -p "echo $prompt " -l confirm
    switch $confirm
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end
