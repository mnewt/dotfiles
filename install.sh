#!/bin/sh
# Run from a dotfile directory, links all files and directories into the current user's home directory

scriptname="install.sh"
scriptbuildnum="0.7"
scriptbuilddate="2016-07-12"


### FUNCTIONS ###

display_ver() {
  echo "$scriptname  ver $scriptbuildnum - $scriptbuilddate"
}

display_help_text () {

  display_ver   # Print script-title, build-number, and build-date

  cat <<-'EOF'

Copies/links files and directories from the current dir to the user's home dir

USAGE: install.sh [OPTIONS] [SOURCE-DIR] [DEST-DIR]
    SOURCE-DIR defaults to the current directory
    DEST-DIR defaults to the current user's home directory

OPTIONS:
  -f (--force)    : force overwrite of files or directories in DEST-DIR
                    (default is false)
  -t (--test)     : test mode - only display changes, don't make them
                    (default is false)
  -c (--config)   : specify a configuration file
                    (default configuration file is `settings`)
                      Example config file contents:
                        # Settings (can use globs)
                        # ignore these files (modifies include)
                        ignore='Icon* *.md *.sh *.txt scripts'
                        # create dir (not contents), then sym-link children (contents)
                        link_children='config'
                        # copy these files
                        copy=''
                        # sym-link these files
                        link='*'

  -h (--help)     : display this help and exit
  -V (--version)  : output version information and exit

  SOURCE-DIR      : directory containing dotfiles to be copied and/or linked
                    (default is current directory)
                      files should NOT have leading '.'
                      example: '.bashrc' should be named 'bashrc' in SOURCE-DIR

  DEST-DIR        : target directory where copies and links will be placed
                    (default is '~')

EOF
}

list_contains() {
  for word in $1; do
    [ "$word" = "$2" ] && return 0
  done
  return 1
}

remove_dupes() {
  unset new_list
  for i in $1; do
    list_contains "$2" "$i" || new_list="$new_list $i"
  done
  echo "$new_list"
}

remove_file() {
  if [ "$testing" = true ]; then
    echo "        TESTING: rm -rf $1"
  elif [ "$force" = true ]; then
    echo "       REMOVING: $1"
    rm -rf "$1"
  else
    echo "NOT OVERWRITING: $1"
    return 1
  fi
}

link_file() {
  if [ "$testing" = true ]; then
    echo "        TESTING: ln -s $1 $2"
  else
    echo "        LINKING: $2"
    ln -s "$1" "$2"
  fi
}

copy_file() {
  if [ "$testing" = true ]; then
    echo "        TESTING: cp -R $1 $2"
  else
    echo "        COPYING: $2"
    cp -R "$1" "$2"
  fi
}

make_dir() {
  if [ "$testing" = true ]; then
    echo "        TESTING: mkdir -p $1"
  else
    echo "       CREATING: $1"
    mkdir -p "$1"
  fi
}

make_links() {
  for s in $1; do
    target="$dest_dir/.$s"
    src="$source_dir/$s"
    # if file (or a link) exists at destination
    if [ -e "$target" ] || [ -L "$target" ]; then
      remove_file "$target" && link_file "$src" "$target"
    else
      link_file "$src" "$target"
    fi
  done
}

full_path () {
  echo "$(cd $1 && pwd)"
}

### PRE-EXECUTION TASKS ###

force=false
testing=false

config_file="$(full_path $(dirname $0))/settings"

# parse arguments
for arg in "$@"; do
  case "$arg" in
    -f|--force)
      force=true
      shift
      ;;
    -t|--testing)
      testing=true
      shift
      ;;
    -c|--config-file)
      shift
      config_file="$1"
      shift
      ;;
    -h|--help)
      display_help_text
      exit
      ;;
    -V|--version)
      display_ver
      exit
      ;;
  esac
done

### VARIABLE INITIALIZATION ###

# Read in settings
. "$config_file"

# Set default dirs (must occur after parameter parsing)
source_dir="${1:-$(pwd)}"
source_dir="$(full_path $source_dir)"
dest_dir="${2:-$HOME}"

echo Using settings:
echo "  config file:      $config_file"
echo "  source dir:       $source_dir"
echo "  destination dir:  $dest_dir"
echo


### MAKE LISTS OF SOURCES ###

# Expand globs
link_sources=$(cd $source_dir && echo $link)
ignore_sources=$(cd $source_dir && echo $ignore)
copy_sources=$(cd $source_dir && echo $copy)
link_children_sources=$(cd $source_dir && echo $link_children)

# Remove duplicate and ignored files from file-lists read from 'sources'
link_sources=$(remove_dupes "$link_sources" "$ignore_sources")
link_sources=$(remove_dupes "$link_sources" "$copy_sources")
link_sources=$(remove_dupes "$link_sources" "$link_children_sources")
copy_sources=$(remove_dupes "$copy_sources" "$ignore_sources")
link_children_sources=$(remove_dupes "$link_children_sources" "$ignore_sources")


### EXECUTE - MAKE LINKS, COPIES, AND DIRS ##

# Make links
make_links "$link_sources"

# Create copies
for s in $copy_sources; do
  target="$dest_dir/.$s"
  src="$source_dir/$s"
  # if file (or a link) exists at destination
  if [ -e "$target" ] || [ -L "$target" ]; then
    remove_file "$target" && copy_file "$src" "$target"
  else
    copy_file "$src" "$target"
  fi
done

# mkdir then link children
for s in $link_children_sources; do
  target="$dest_dir/.$s"
  children=$(echo $s/*)
  if [ -e "$target" ] || [ -L "$target" ]; then
    remove_file "$target" && make_dir "$target" && make_links "$children"
  else
    make_dir "$target" && make_links "$children"
  fi
done
