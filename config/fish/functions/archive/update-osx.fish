function update-osx -d 'Update Mac OS X packages'
  if installed softwareupdate
    sudo softwareupdate -i -a
  end
end
