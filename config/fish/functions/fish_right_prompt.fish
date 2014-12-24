function fish_right_prompt --description "Write out the right prompt"
  # Job count
  set -l job_info
  set -l job_count (jobs -c | wc -l | awk '{ print $1; }')
  if [ $job_count -gt 0 ]
    echo -ns "$magenta" $job_count " job"
    if [ $job_count -gt 1 ]
      # make jobs plural
      echo -ns "s"
    end
    echo -ns "$normal"
  end
end