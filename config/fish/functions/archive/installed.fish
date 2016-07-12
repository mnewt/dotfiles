# Succeed if the given utility is installed. Fail otherwise.
# For explanations about `which` vs `type` vs `command`, see:
# http://stackoverflow.com/questions/592620/check-if-a-program-exists-from-a-bash-script/677212#677212
function installed -d 'check if a utility is in the $PATH'
  command -v $argv >/dev/null 2>&1
end
