# Filenames:
regexp=[-\w\.]+:\d+:?|^[-\w\.\[\]]+:
colours=yellow
..
# Errors and warnings:
regexp=error:.*
colours=red
..
regexp=error:
colours=bold red
..
regexp=undefined reference to.*
colours=red bold
..
regexp=(?<=undefined reference )to
colours=red
...
regexp=\*\*\* No rule to make target.*
colours=red
..
regexp=ld returned 1 exit status
colours=bold red
..
regexp=\*\*\*
colours=red bold
..
regexp=warning:.*
colours=bold yellow
..
regexp=warning:
colours=bold yellow
..
regexp=Error \d+
colours=bold red
..

# Notable cc options:
regexp=\B-(g|O\d|o)\b
colours=green
..
# Color filenames for -o options.
regexp=(?<=-[o] )[-\w\./]+
colours=green bold
..
# Make any directory part of the above filename not bold
regexp=(?<=-o )[-\w\./]+/
colours=green 
..

# CC calls:
regexp=^(g\+\+|gcc|cc)
colours=green bold

..
# Common success messages:
regexp=(Build completed)|[^:]+is up to date
colours=green bold
