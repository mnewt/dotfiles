function ips -d 'Print the computers IP addresses'

  # OS Specific
  switch (uname)
    case Linux
      ip addr show | grep -B1 "inet" | awk -F' ' '{ if ( $1 == "inet" ) { print $2 } else if ( $2 == "Link" ) { printf "%s:" ,$1 } }' | awk -F: '{ print $1 ": " $3 }'
    case Darwin
      for i in (ifconfig -l | tr ' ' '\n')
        set -l ipaddr (ifconfig $i | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)')
        # this test is always true
        if test -n "$ipaddr"
          echo $i ":" $ipaddr
        end
      end
    case 'CYGWIN*'
      ipconfig | awk -F" ." '/Address/ {print $NF}'
  end
end

# This might be a good way to parse OSX ifconfig, and could be adapted for Linux
# ifconfig -a | awk '$2~/^flags/{_1=$1;getline;if($1~/^inet/){print _1" "$2}}'