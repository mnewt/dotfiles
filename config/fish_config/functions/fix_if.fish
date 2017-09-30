function fix_if -d "If file exists, then source with fix"
  if functions -q fix
    if test -e "$argv[1]"
      fix source "$argv[1]"
    end
  else
    echo "fix is not installed -- you should probably run `fisher`"
  end
end
