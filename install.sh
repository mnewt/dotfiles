#!/bin/bash
# Run from a dotfile directory, links all files and directories into the current user's home directory

# TODO
#
# color output

# ignore these files when creating links
ignore_list='README.md install.sh TODO.txt apps'

read -r -d '' help_text <<-'EOF'
install.sh version 0.4

Run from a dotfile directory, links all files and directories into the current user's home directory

./install.sh [-f] [-h] [-t] [source dir] [destination dir]

  -f (--force)    : Overwrite any files / directories in the destination directory
                    (default is false)
  -h (--help)     : This help message
  -t (--test)     : Don't actually do anything, just show what would be done
                    (default is false)

  source dir      : Contains dotfiles. They are expected to NOT have leading '.' For example,
                    if the dotfile is '.bashrc' then in the source dir it is 'bashrc'
                    (default is current directory)
  destination dir : Where to put symlinks
                    (default is '~')
EOF

function list_contains() {
  for word in $1; do
    [ "$word" = "$2" ] && return 0
  done
  return 1
}

function link_file() {
  if [ "$testing" == true ]; then
    echo TESTING: ln -s "$1" "$2"
  else
    ln -s "$1" "$2"
  fi
}

# initialize
force=false
testing=false

# parse arguments
for arg in $@; do
  case "$arg" in
    -f|--force)
      force=true
      shift
      ;;
    -t|--testing)
      testing=true
      shift
      ;;
    -h|--help)
      echo "$help_text"
      exit
      ;;
  esac
done

source_dir="${1-$(pwd)}"
dest_dir="${2-$HOME}"

# echo "force: $force"
# echo "testing: $testing"
# echo "source_dir: $source_dir"
# echo "dest_dir: $dest_dir"

# echo "arguments left over: $@"

for source in $source_dir/*; do
  # ignore non-rc files
  if ! list_contains "$ignore_list" "$(basename $source)"; then
    target="$dest_dir/.$(basename $source)"
    # if target (or a dead link) exists
    if [ -e "$target" ] || [ -L "$target" ]; then
      if [ "$force" = true ]; then
        echo "REPLACING: $target"
        if [ "$testing" = true ]; then
          echo TESTING: rm -rf "$target"
        else
          rm -rf "$target"
        fi
        link_file "$source" "$target"
      else
        echo "NOT OVERWRITING: $target"
      fi
    else
      link_file "$source" "$target"
    fi
  fi
done
