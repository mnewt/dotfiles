#!/bin/sh

for name in *; do
  target="$HOME/.$name"
  if [ -e "$target" ] && [ "$1" != '-f' ]; then
    echo "WARNING: not creating $target because it already exists. Use '-f' to force overwrite"
  else
    if [ "$name" != 'install.sh' ] && [ "$name" != 'README.md' ]; then
      if [ -e "$target" ] && [ "$1" == '-f' ]; then
        echo "Replacing $target"
        rm -rf "$target"
      else
        echo "Creating $target"
      fi
      ln -s "$PWD/$name" "$target"
    fi
  fi
done
