function realtime
	set -l command_start_time (date '+%s%N')
	eval "$argv"
	set -l command_end_time (date '+%s%N')
	set -l command_duration (math "$command_end_time - $command_start_time")

	set -l minutes (math "$command_duration / 60000000000")
	set -l seconds (math "$command_duration % 60000000000 / 1000000000")
	set -l milliseconds (math "$command_duration % 1000000000 / 1000000")

	printf '\nreal\t%dm%d.%03ds\n' $minutes $seconds $milliseconds
end
