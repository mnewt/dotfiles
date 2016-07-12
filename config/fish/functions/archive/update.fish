function update -d 'Update software on the computer'
  # search for all update-*.fish scripts and run them in succession
  for f in (string match -r '[^\/]+(?=\.fish$)' $HOME/.config/fish/functions/update-*.fish)
    echo -s (set_color -o magenta) "==> " (set_color -o FFF) (desc "$f") (set_color normal)
    eval $f $argv
  end
end
