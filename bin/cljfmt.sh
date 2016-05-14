#!/bin/sh
# Use cljfmt (https://github.com/weavejester/cljfmt)
# to beautify a file or all files in a directory

if [ "argv[1]" == "-h" ]; then
  echo "Usage: cljfmt [FILE | DIRECTORY]"
  echo "Beautify a Clojure file or all files in a directory"
fi

boot 
