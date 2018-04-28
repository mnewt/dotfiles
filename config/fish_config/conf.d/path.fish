for p in "$HOME/.bin" "/usr/local/opt/texinfo/bin" "$HOME/.cargo/bin" "$HOME/Library/Python/3.6/bin" "$GOPATH/bin" "/usr/local/sbin" "/usr/local/opt/coreutils/libexec/gnubin" $PATH
  if test -d "$p"; and not contains "$p" $new_path
    set new_path $new_path "$p"
  end
end

set PATH $new_path
