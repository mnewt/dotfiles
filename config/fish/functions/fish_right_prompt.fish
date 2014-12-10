function fish_right_prompt --description "Write out the right prompt"
  # Job count
  set -l job_info
  set -l job_count (jobs -c | wc -l | awk '{ print $1; }')
  if [ $job_count -gt 0 ]
    if [ $job_count -eq 1 ]
      set job_info "$magenta""($job_count job)"
    else
      set job_info "$magenta""($job_count jobs)"
    end
  end

  echo $job_info
end