#!/bin/sh
# Run from a dotfile directory, links all files and directories into the current user's home directory

# Settings
# (you can use globs)

# list of files to link
link='*'

# ignore these files (modifies include)
ignore='Icon* *.md *.sh *.txt scripts'

# just copy these files
copy=''

# create directory itself (not contents), then link the children of the directory
# NOTE: it will not delete the directory. If you need to do that, do it manually
link_children='config'



read -r -d '' help_text <<-'EOF'
install.sh version 0.5

Run from a dotfile directory, links all files and directories into the current
user's home directory

./install.sh [-f] [-h] [-t] [-c config-file] [source dir] [destination dir]

  -f (--force)    : Overwrite any files / directories in the destination dir
                    (default is false)
  -h (--help)     : This help message
  -t (--test)     : Don't actually do anything, just show what would be done
                    (default is false)
  -c (--config)   : Specify configuration file. Example file contents:
                    # Settings
                    # (you can use globs)
                    # list of files to link
                    link='*'
                    # ignore these files (modifies include)
                    ignore='Icon* *.md *.sh *.txt scripts'
                    # just copy these files
                    copy=''
                    # create directory itself (not contents), then link the
                    # children of the directory
                    link_children='config'
                    (defaults are in `install.sh`)

  source dir      : Contains dotfiles. They are expected to NOT have leading
                    '.' For example, if the dotfile is '.bashrc' then in the
                    source dir it is 'bashrc'
                    (default is current directory)
  destination dir : Where to put symlinks
                    (default is '~')
EOF


# functions

function list_contains() {
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

function remove_file() {
  if [ "$testing" = true ]; then
    echo "TESTING: rm -rf $1"
  elif [ "$force" = true ]; then
    echo "REMOVING: $1"
    rm -rf "$1"
  else
    echo "NOT OVERWRITING: $1"
    return 1
  fi
}

function link_file() {
  if [ "$testing" = true ]; then
    echo "TESTING: ln -s $1 $2"
  else
    echo "LINKING: $2"
    ln -s "$1" "$2"
  fi
}

function copy_file() {
  if [ "$testing" = true ]; then
    echo "TESTING: cp -R $1 $2"
  else
    echo "COPYING: $2"
    cp -R "$1" "$2"
  fi
}

function make_dir() {
  if [ "$testing" = true ]; then
    echo "TESTING: mkdir -p $1"
  else
    echo "CREATING: $1"
    mkdir -p "$1"
  fi
}


# initialize flags
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
    -c|--config-file)
      shift
      source $1
      shift
      ;;
    -h|--help)
      echo "$help_text"
      exit
      ;;
  esac
done


# Organize data

# Defaults
source_dir="${1-$(pwd)}"
dest_dir="${2-$HOME}"

# Expand globs
link_sources=$(echo $link)
ignore_sources=$(echo $ignore)
copy_sources=$(echo $copy)
link_children_sources=$(echo $link_children)

# Remove files that shouldn't be linked
link_sources=$(remove_dupes "$link_sources" "$ignore_sources")
link_sources=$(remove_dupes "$link_sources" "$copy_sources")
link_sources=$(remove_dupes "$link_sources" "$link_children_sources")


# Do it

# Create links
function make_links() {
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
make_links "$link_sources"

# Create copies
for s in $copy_sources; do
  target="dest_dir/.$s"
  src="$source_dir/$s"
  # if file (or a link) exists at destination
  if [ -e "$target" ] || [ -L "$target" ]; then
    remove_file "$target" && link_copy "$src" "$target"
  else
    copy_file "$src" "$target"
  fi
done

# mkdir then link children
for s in $link_children_sources; do
  target="$dest_dir/.$s"
  children=$(echo $s/*)
  make_dir "$target"
  make_links "$children"
done