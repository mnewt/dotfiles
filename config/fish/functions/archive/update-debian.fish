function update-debian -d 'Update Debian/Ubuntu packages'
  if begin; test -e /etc/debian_version; or test -e /etc/lsb_release; end
    sudo apt update
    sudo apt upgrade -y
    sudo apt-get autoremove -y
    sudo apt-get autoclean -y
  end
end
