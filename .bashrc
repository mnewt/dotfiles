#!/bin/bash
# Runs in interactive, non-login shells

. $HOME/.bin/shell_utils

# locale
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_COLLATE=C

paths=(
	/sbin
	/usr/sbin
	/bin
	/usr/bin
	/Library/Apple/bin
	/Library/Apple/usr/bin
	/opt/X11/bin
	/usr/local/sbin
	/usr/local/bin
	/opt/local/bin
	/opt/local/sbin
	/usr/local/opt/coreutils/libexec/gnubin
	/usr/local/opt/texinfo/bin
	/Library/TeX/texbin
	/usr/local/opt/gnutls/bin
	/usr/local/Cellar/poppler/*/bin
	/usr/local/opt/sqlite/bin
	$HOME/.cargo/bin
	$GOPATH/bin/usr/local/sbin
	/usr/local/opt/llvm/bin
	/usr/local/Cellar/perl/*/bin
	/usr/local/opt/ruby/bin
	$HOME/.gem/ruby/*/bin
	/usr/local/opt/python/libexec/bin
	$HOME/Library/Python/*/bin
	$HOME/.local/bin
	$HOME/.bin
)

pkg_config_paths=(
	/usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/Aliases/pkgconfig
	/usr/local/Homebrew/Library/Homebrew/os/mac/pkgconfig
	/usr/local/opt/ruby/lib/pkgconfig
	/usr/local/lib/pkgconfig
	/usr/local/share/pkgconfig
	/usr/lib/pkgconfig
)

man_paths=(
	/opt/local/share/man
	/usr/local/share/man
	/usr/share/man
)

add_to_path PATH "${paths[@]}"
add_to_path PKG_CONFIG_PATH "${pkg_config_paths[@]}"
add_to_path MANPATH "${man_paths[@]}"

# python venv
export VIRTUAL_ENV_DISABLE_PROMPT=true

# boot-clj
export BOOT_JVM_OPTIONS='-client -XX\:+TieredCompilation -XX\:TieredStopAtLevel\=1 -Xmx2g -XX\:+UseConcMarkSweepGC -XX\:+CMSClassUnloadingEnabled -XX\:+AggressiveOpts'

source_if "$HOME/.private.sh"
