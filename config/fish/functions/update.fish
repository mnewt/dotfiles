function update -d 'Update software on the computer'
  # search for all update-*.fish scripts and run them in succession
  for f in (string match -r '[^\/]+(?=\.fish$)' $HOME/.config/fish/functions/update-*.fish)
    echo -s (set_color -o white) "==> " (set_color -o green) "$f" (set_color normal)
    eval $f $argv
  end
end
