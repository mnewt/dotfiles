function update-dotfiles -d 'Update my dotfiles from the git repository'
  set -l dotfiles_locations "$HOME/.dotfiles" "$HOME/dotfiles"
  set -l dotfiles
  for loc in $dotfiles_locations
    test -d "$loc"; and set dotfiles "$loc"
  end
  echo "Checking for git updates at dotfiles location: $dotfiles"
  if count (git -C "$dotfiles" ls-files --exclude-standard --others) >/dev/null
    echo "Not updating dotfiles because the repo is dirty"
    return 1
  else
    git -C "$dotfiles" pull
  end
end
