function update-gems -d 'Update Ruby packages'
  if installed gem
    update_rubygems
    sudo gem update --system
    gem clean
  end
end
