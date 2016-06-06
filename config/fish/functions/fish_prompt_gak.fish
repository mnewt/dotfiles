function _common_section
    printf $c1
    printf $argv[1]
    printf $c0
    printf ":"
    printf $c2
    printf $argv[2]
    printf $argv[3]
    printf $c0
    printf ", "
end

function section
    _common_section $argv[1] $c3 $argv[2] $ce
end

function error
    _common_section $argv[1] $ce $argv[2] $ce
end

function fish_prompt
    # $status gets nuked as soon as something else is run, e.g. set_color
    # so it has to be saved asap.
    set -l last_status $status

    # c0 to c4 progress from dark to bright
    # ce is the error colour
    set -g c0 (set_color 005284)
    set -g c1 (set_color 0075cd)
    set -g c2 (set_color 009eff)
    set -g c3 (set_color 6dc7ff)
    set -g c4 (set_color ffffff)
    set -g ce (set_color $fish_color_error)

    # Clear the line because fish seems to emit the prompt twice. The initial
    # display, then when you press enter.
    printf "\033[K"

    # Current time
    printf (date "+$c2%H$c0:$c2%M$c0:$c2%S, ")
    if [ $last_status -ne 0 ]
        error last $last_status
        set -ge status
    end

    # Track the last non-empty command. It's a bit of a hack to make sure
    # execution time and last command is tracked correctly.
    set -l cmd_line (commandline)
    if test -n "$cmd_line"
        set -g last_cmd_line $cmd_line
        set -ge new_prompt
    else
        set -g new_prompt true
    end

    # Show last execution time and growl notify if it took long enough
    set -l now (date +%s)
    if test $last_exec_timestamp
        set -l taken (math $now - $last_exec_timestamp)
        if test $taken -gt 10 -a -n "$new_prompt"
            error taken $taken
            echo "Returned $last_status, took $taken seconds" | \
                growlnotify -s $last_cmd_line
            # Clear the last_cmd_line so pressing enter doesn't repeat
            set -ge last_cmd_line
        end
    end
    set -g last_exec_timestamp $now

    # Show loadavg when too high
    set -l load1m (uptime | grep -o '[0-9]\+\.[0-9]\+' | head -n1)
    set -l load1m_test (math $load1m \* 100 / 1)
    if test $load1m_test -gt 100
        error load $load1m
    end

    # Show disk usage when low
    set -l du (df / | tail -n1 | sed "s/  */ /g" | cut -d' ' -f 5 | cut -d'%' -f1)
    if test $du -gt 80
        error du $du%%
    end

    # Virtual Env
    if set -q VIRTUAL_ENV
        section env (basename "$VIRTUAL_ENV")
    end

    # Git branch and dirty files
    git_branch
    if set -q git_branch
        set out $git_branch
        if test $git_dirty_count -gt 0
            set out "$out$c0:$ce$git_dirty_count"
        end
        section git $out
    end

    # Current Directory
    # 1st sed for colourising forward slashes
    # 2nd sed for colourising the deepest path (the 'm' is the last char in the
    # ANSI colour code that needs to be stripped)
    printf $c1
    printf (pwd | sed "s,/,$c0/$c1,g" | sed "s,\(.*\)/[^m]*m,\1/$c3,")

    # Prompt on a new line
    printf $c4
    printf "\n> "
end