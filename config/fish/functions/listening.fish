function listening -d 'Print the currently listening IP ports'
  # OS Specific
  switch (uname)
    case Linux
      ss -lnptu
    case Darwin
      lsof -i -n -P | grep LISTEN
    case 'CYGWIN*'
      netstat -an | grep LISTENING
  end
end