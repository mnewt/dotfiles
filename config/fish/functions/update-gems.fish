function update-gems -d 'Update Ruby packages'
  if installed gem
    sudo gem update --system
    gem clean
  end
end
