set -g fin_version 7
complete -xc fin -n __fish_use_subcommand -a version -d "Show version info"
complete -xc fin -n __fish_use_subcommand -a help  -d "Show usage help"

function fin -d "fish plugin manager"
    if test -z "$XDG_CACHE_HOME"
        set XDG_CACHE_HOME ~/.cache
    end

    if test -z "$XDG_CONFIG_HOME"
        set XDG_CONFIG_HOME ~/.config
    end

    set -g fin_cache "$XDG_CACHE_HOME/fin"
    set -g fin_config "$XDG_CONFIG_HOME/fin"
    set -l fish_config "$XDG_CONFIG_HOME/fish"

    if test -z "$fish_path"
        set -g fish_path "$fish_config"
    end

    set -l fin (status -f)
    set -l fishfile "$fish_path/fishfile"

    switch "$argv"
        case -v {,-,--}version
            echo "fin version $fin_version" (command uname) (fish_realpath "$fin" | string replace ~ \~)
            return 0
        case -h {,-,--}help
            _fin_help > /dev/stderr
            return
        case -- ""
        case \*
            echo "fin: Unknown flag or command \"$i\""
            _fin_help > /dev/stderr
            return 1
    end

    command mkdir -p "$fin_cache" {"$fish_path","$fish_config"}/{conf.d,functions,completions}
    or return 1

    set -l config "$fin_config"/*/*

    if test -s "$fishfile"
        if test "$fin" = "$fish_config/functions/fin.fish" -a (perl -e "printf(\"%s\n\", time - (stat ('$fin'))[9])") -gt 3600
            curl -s --max-time 5 "https://raw.githubusercontent.com/fisherman/fin/master/fin.fish?nocache" > "$fin@"

            if test -s "$fin@"
                command mv -f "$fin@" "$fin"
                source "$fin"
            end
            command rm -f "$fin@"
        end
        
        _fin_fmt < "$fishfile" > "$fishfile@"
        command cat "$fishfile@" > "$fishfile"
        command rm -f "$fishfile@"

    else if test -z "$config"
        _fin_help > /dev/stderr
        return 1
    end

    for i in $config
        _fin_plugin_unload "$i"
    end

    command rm -rf "$fin_config"
    command mkdir -p "$fin_config"

    _fin_fetch (_fin_read < "$fishfile")
end

function _fin_help
    set -l c (set_color $fish_color_command) (set_color normal)
    echo "Edit $c[1]\$fish_path$c[-1]/fishfile to add or remove plugins."
    echo "When you're done, run $c[1]fin$c[-1] to commit changes."
end

function _fin_fetch
    set -l j
    set -l plugins
    set -l fetched

    for i in $argv
        switch "$i"
            case \~\* .\* /\*
                if set -l path (realpath (string replace \~ ~ "$i") ^ /dev/null)
                    set links $links $path
                else
                    echo "fin: Invalid path: $i" > /dev/stderr
                end
                continue
        end

        set i (string split @ "$i")
        set -q i[2]
        or set i $i "master"

        fish -c "
            command mkdir -p \"$fin_config/$i[1]\"
            if curl --max-time 2 -Ss \"https://codeload.github.com/$i[1]/tar.gz/$i[-1]\" ^ /dev/stdout | tar -xzf- -C \"$fin_config/$i[1]\" --strip-components=1 ^ /dev/null
                command mkdir -p \"$fin_cache/$i[1]\"
                command cp -Rf \"$fin_config/$i[1]\" \"$fin_cache/$i[1]/..\"

            else if test -d \"$fin_cache/$i[1]\"
                command cp -Rf \"$fin_cache/$i[1]\" \"$fin_config/$i[1]/..\"
            else
                command rm -rf \"$fin_config/$i[1]\"
                echo \"fin: Fetch error $i[1]\" > /dev/stderr
            end
        " &

        set j $j (_fin_jobs -l)
        set plugins $plugins "$i[1]"
    end

    if test ! -z "$j"
        _fin_wait $j
        for i in $plugins
            if test -d "$fin_config/$i"
                set fetched $fetched "$i"
                _fin_plugin_load "$fin_config/$i"
            end
        end
    end

    for i in $links
        set fetched $fetched "$i"
        set -l dir (string split / "$i")[-1]
        command mkdir -p "$fin_config/$USER"
        command ln -sf "$i" "$fin_config/$USER/$dir"
        _fin_plugin_load "$fin_config/$USER/$dir"
    end

    if test ! -z "$fetched"
        _fin_fetch (_fin_missing $fetched | command sort -u)
    end
end

function _fin_missing
    for i in $argv
        set -l path "$i"
        if test ! -d "$path"
            set path "$fin_config/$i"
        end
        if test ! -d "$path"
            printf "%s\n" "$i"
        else if test -s "$path/fishfile"
            _fin_missing (_fin_read < "$path/fishfile")
        end
    end
end

function _fin_plugin_load -a plugin
    for i in "$plugin/"{,functions,conf.d,completions}/*.fish
        set -l target (string split / "$i")[-1]
        switch "$i"
            case $plugin/conf.d\*
                set target "$fish_path/conf.d/$target"
            case $plugin/completions\*
                set target "$fish_path/completions/$target"
            case $plugin/{,functions}\*
                set target "$fish_path/functions/$target"
        end
        command ln -f "$i" "$target"
        source "$target" ^ /dev/null
    end
end

function _fin_plugin_unload -a plugin
    for i in "$plugin/"{,functions,conf.d,completions}/*.fish
        set -l target (string split / "$i")[-1]
        set -l cmd (string replace ".fish" "" "$target")
        switch "$i"
            case $plugin/conf.d\*
                command rm -f "$fish_path/conf.d/$target"
            case $plugin/completions\*
                command rm -f "$fish_path/completions/$target"
                complete -ec "$cmd"
            case $plugin/{,functions}\*
                command rm -f "$fish_path/functions/$target"
                functions -e "$cmd"
        end
    end
    functions -q fish_prompt
    or source "$__fish_datadir/functions/fish_prompt.fish"
end

function _fin_read
    command awk '!/#/'
end

function _fin_fmt
    command awk '
        function swap(list, i, j,   x) {
            x = list[i]
            list[i] = list[j]
            list[j] = x
        }

        function sort(list, lo, hi,   i, n) {
            if (lo >= hi) return
            swap(list, lo, int((lo + hi) / 2))
            n = lo
            for (i = lo + 1; i <= hi; i++) {
                if (list[i] < list[lo]) {
                    swap(list, ++n, i)
                }
            }
            swap(list, lo, n)
            sort(list, lo, n - 1)
            sort(list, n + 1, hi)
        }

        /^$/ { next } { gsub(/^[ \t]*|[ \t]*$|\.git$|^(https?:\/\/)?github\.com\//, "") }
        {
            if (/^#/) {
                isComment = 1
                sub(/#+[ \t]*/, "")
            }
            gsub(/[ \t#]+.*/, "")
            if (uniq[$0]++) {
                next
            }
            if (isComment) {
                aComments[++nComments] = $0
                isComment = 0
            }
            aItems[++nItems] = $0
        }
        END {
            sort(aItems, 1, nItems)
            for (i = 1; i <= nItems; i++) {
                for (j = 1; j <= nComments; j++) {
                    if (aItems[i] == aComments[j]) {
                        aItems[i] = "# " aItems[i]
                        break
                    }
                }
                if (aItems[i] ~ /^(# )?~/) {
                    print(aItems[i])
                    continue
                }
                plugins[++nPlugins] = aItems[i]
            }
            for (i = 1; i <= nPlugins; i++) {
                print(plugins[i])
            }
        }
    '
end

function _fin_jobs
	jobs $argv | command awk -v FS=\t '
        /[0-9]+\t/{
            jobs[++nJobs] = $1
        }
        END {
            for (i in jobs) {
                print(jobs[i])
            }
            exit nJobs == 0
        }
    '
end

function _fin_wait
    while true
        set -l has_jobs
        set -l all_jobs (_fin_jobs)
        or break

        for j in $argv
            if contains -- $j $all_jobs
                set -e has_jobs
                break
            end
        end

        if set -q has_jobs
            break
        end
    end
end
