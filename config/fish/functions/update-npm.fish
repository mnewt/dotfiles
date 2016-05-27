function update-npm -d 'Update Node.js packages'
  if installed npm
    npm install npm -g
    npm update -g
  end
end
