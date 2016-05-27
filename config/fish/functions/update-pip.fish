function update-pip -d 'Update Python packages'
  if installed pip
    sudo -H pip install --upgrade pip
    pip list --outdated | cut -d ' ' -f 1 | sudo -H xargs -n1 pip install --upgrade
  end
end
