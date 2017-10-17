# Homebrew installs to /usr/local/sbin and /usr/local/opt/coreutils/libexec/gnubin
for p in  "/usr/local/sbin" "/usr/local/opt/coreutils/libexec/gnubin" "$HOME/.bin" "$GOPATH/bin"
  if test -d "$p"; and not contains "$p" $PATH
    set PATH "$p" $PATH
  end
end
