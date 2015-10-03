# Recursively delete OS cache files
# PROBABLY IS DANGEROUS!
function clean-os-junk
  set -l files_to_delete '*.DS_Store' 'desktop.ini' 'Icon\r'
  if set -q argv
    set dir '.'
  else
    set dir $argv
  end
  for file in $files_to_delete
    find "$dir" -name "$file" -print
  end
  while true
    read -l -p 'echo "Do you wish to delete these files? [yN] "' yn
    set yn (echo $yn | tr '[:upper:]' '[:lower:]')
    switch $yn
      case 'y*'
        for file in $files_to_delete
          find "$dir" -name "$file" -print -delete
        end
        return 0
      case '*'
        echo 'Exiting, no files modified'
        return 0
    end
  end
end