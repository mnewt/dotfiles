# PROBABLY IS REALLY DANGEROUS!
function clean-os-junk -d 'Recursively delete OS directory cache files'
  set -l files_to_delete '*.DS_Store' 'desktop.ini' 'Icon\r' 'thumbs.db'
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
        echo 'Deleting files...'
        for file in $files_to_delete
          find "$dir" -name "$file" -print -delete >/dev/null
        end
        return 0
      case '*'
        echo 'Exiting, no files modified'
        return 0
    end
  end
end
