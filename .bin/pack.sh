#!/usr/bin/env bash

pack_usage () {
  cat <<EOF
pack - For when you want to manage your package management,
       modularly and idempotently

  pack [COMMAND | MODULE]

COMMANDS:

  help     This helpful message
  list     List modules

EOF
}

namespace_module () {
  sed 's/^\([a-zA-Z_][a-zA-z_0-9]*\)[ ]* ()[ ]*{[ ]*$/'"$(basename "$1")"'_\1 () {/g' "$1"
}

contains () {
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}

list_modules () {
  for m in "${pack_modules[@]}"; do
    echo "$m"
  done
}

module_list () {
  if contains "$1" "${pack_modules[@]}"; then
    "${1}_list"
  else
    echo "Module not found ($1)"
  fi
}

pack_section () {
  for line in $(sed -n "/\[${2}\]/,/\[[a-z]+\]/p" "$pack_dir/packages/$1"); do
    echo $line
  done
}

pack_diff_left () {
  for e in $1; do
    contains "$e" $2 
  done
}

pack_dir="${PACK_DIR:-$HOME/.pack}"
pack_modules=()

[ -e "$pack_dir/modules" ] || mkdir -p "$pack_dir/{modules,packages}"

for m in "$pack_dir/modules/"*; do
  eval "$(namespace_module "$m")" && pack_modules+=("$(basename "$m")")
done

list () {
  case "$1" in
    '')
      list_modules ;;
    *)
      module_list "$@" ;;
  esac
}

install () {
  for m in "${pack_modules[@]}"; do
    echo "Installing packages for: ($m)..."
    "${m}_install"
  done
}

main () {
  case "$1" in
    help|-h|--help)
      pack_usage ;;
    list)
      shift
      list "$@" ;;
    *)
      if contains "$1" "${pack_modules[@]}"; then
        local module="$1"
        shift
        "${module}_install"
      else
        pack_usage
        exit 26
      fi ;;
  esac
}

# main "$@"

pack_section homebrew recipes 
