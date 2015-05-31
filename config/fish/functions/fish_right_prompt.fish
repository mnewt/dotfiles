function fish_right_prompt --description "Write out the right prompt"
  # Job count
  set -l job_count (jobs -c | wc -l | awk '{ print $1; }')
  if [ $job_count -gt 0 ]
    echo -ns "$magenta" $job_count " job"
    if [ $job_count -gt 1 ]
      # make jobs plural
      echo -ns "s"
    end
    echo -ns "$normal"
  end

  # tmux sessions
  set -l session_count (tmux ls 2>/dev/null | wc -l | awk '{ print $1; }')
  if [ $session_count -gt 0 ]
    echo -ns "  $cyan" $session_count " session"
    if [ $session_count -gt 1 ]
      # make sessions plural
      echo -ns "s"
    end
    echo -ns "$normal"
  end
end