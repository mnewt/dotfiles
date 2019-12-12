#!/bin/bash

. $HOME/.bin/bash_utils

# locale
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_COLLATE=C

paths=(
	$HOME/.bin
	$HOME/.local/bin
	$(list_sort_reverse $HOME/Library/Python/*/bin)
	/usr/local/opt/python/libexec/bin
	$(list_sort_reverse $HOME/.gem/ruby/*/bin)
	/usr/local/opt/ruby/bin
	$(list_sort_reverse /usr/local/Cellar/perl/*/bin)
	/usr/local/opt/llvm/bin
	$GOPATH/bin/usr/local/sbin
	$HOME/.cargo/bin
	$HOME/.dotnet/tools
	/usr/local/share/dotnet
	/Library/Frameworks/Mono.framework/Versions/Current/Commands
	/Library/Frameworks/Mono.framework/Versions/Current/bin
	/usr/local/opt/sqlite/bin
	/usr/local/Cellar/poppler/*/bin
	/usr/local/opt/gnutls/bin
	/Library/TeX/texbin
	/usr/local/opt/texinfo/bin
	/usr/local/opt/coreutils/libexec/gnubin
	/opt/local/sbin
	/opt/local/bin
	/usr/local/bin
	/usr/local/sbin
	/opt/X11/bin
	/Library/Apple/usr/bin
	/Library/Apple/bin
	/usr/bin
	/bin
	/usr/sbin
	/sbin
)

pkg_config_paths=(
	/usr/lib/pkgconfig
	/usr/local/share/pkgconfig
	/usr/local/lib/pkgconfig
	/usr/local/opt/ruby/lib/pkgconfig
	/usr/local/Homebrew/Library/Homebrew/os/mac/pkgconfig
	/usr/local/Homebrew/Library/Taps/homebrew/homebrew-core/Aliases/pkgconfig
)

man_paths=(
	/usr/share/man
	/usr/local/share/man
	/opt/local/share/man
)

add_to_path PATH "${paths[@]}"
add_to_path PKG_CONFIG_PATH "${pkg_config_paths[@]}"
add_to_path MANPATH "${man_paths[@]}"

# python venv
export VIRTUAL_ENV_DISABLE_PROMPT=true

# boot-clj
export BOOT_JVM_OPTIONS='-client -XX\:+TieredCompilation -XX\:TieredStopAtLevel\=1 -Xmx2g -XX\:+UseConcMarkSweepGC -XX\:+CMSClassUnloadingEnabled -XX\:+AggressiveOpts'

source_if "$HOME/.private.sh"
