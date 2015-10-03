function flush -d 'Flush the computers DNS cache'
  # OS Specific
  switch (uname)
    case Linux
      
    case Darwin
    	# before OSX 10.10.4
      # sudo discoveryutil mdnsflushcache
      # sudo discoveryutil udnsflushcache
      # after OSX 10.10.4
      sudo dscacheutil -flushcache
      sudo killall -HUP mDNSResponder
    case 'CYGWIN*'
      ipconfig /flushdns
  end
end