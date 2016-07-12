function update-arch -d 'Update Arch Linux packages'
  if test -e /etc/arch-release
    sudo /usr/bin/pacman -Syu --noconfirm
    if sudo /usr/bin/pacman -Qtdq > /dev/null
      sudo /usr/bin/pacman -Rns (/usr/bin/pacman -Qtdq | sed -e ':a;N;\$!ba;s/\n/ /g')
    end
    sudo paccache -r
    sudo paccache -ruk0
    sudo pacman-optimize
  end
end
