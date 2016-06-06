function time_it
  set -l n 1
  if test "$argv[1]" = "-n"
    set n $argv[2]
    set -e argv[1..2]
  end
  set -l start (date +"%s")
  for i in (seq $n)
    eval "$argv"
  end
  set -l finish (date +"%s")
  echo "Elapsed seconds:" (math $finish - $start)
end
