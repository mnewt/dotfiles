function update-atom -d 'Update Atom Editor'
  if installed apm
    apm upgrade
    apm clean
    apm list --bare --installed --dev false > ~/.atom/packages.txt
  end
end
