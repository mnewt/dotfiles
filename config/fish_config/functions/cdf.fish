function cdf
  set fpath (osascript -e '
  tell app "finder"
   try
      set folderPath to (folder of the front window as alias)
   on error
      set folderPath to (path to desktop folder as alias)
   end try
   POSIX path of folderPath
  end tell'
  )
  echo "cd $fpath"
  cd "$fpath"
end
