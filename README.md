Some dotfiles
===================

Targeted at [fish shell](http://fishshell.com/)

Older `zsh` dotfiles are still present at this time

Install
=======

[Fork this repo](https://github.com/mnewt/dotfiles) on Github.

Clone your fork (replace `your-github-name` with your Github name).

    git clone git@github.com:your-github-name/dotfiles.git ~/.dotfiles
    cd ~/.dotfiles

Examine the settings file, named `settings`:

```bash
# Settings
# This file tells install.sh what to ignore, link, copy, and create
# List files (and globs) separated by spaces
# The below options are in order of priority, so ignore overrides
# link_children, which overrides copy, which overrides link
# That is why it's OK to just specify '*' for link

# ignore these files (modifies include)
ignore='settings scripts Icon* *.md *.sh *.txt'

# create directory itself (not contents), then link the children of the directory
# NOTE: it will not delete the directory. If you want to replace the directory,
#				delete it first, then run install.sh
link_children='config'

# just copy these files
copy=''

# list of files to link
link='*'
```


Run the installer.

    ./install.sh

It creates symlinks for all dotfiles in your home directory. You can safely run
this file multiple times to update. you may need to use the `--force` option to
get the results you want.

Included are `fish` dotfiles. To install and switch your shell to `fish` on OS X:

    # Install GNU coreutils, fish, grc, and vimpager
    brew coreutils install fish grc vimpager
    echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
    chsh -s /usr/local/bin/fish

And then run fish

    fish -l
    fish_update_completions

Command reference for `install.sh`:
-----------------------------------

		install.sh version 0.5

		Run from a dotfile directory, links all files and directories into the current
		user's home directory

		./install.sh [-f] [-h] [-t] [-c config-file] [source dir] [destination dir]

		  -f (--force)    : Overwrite any files / directories in the destination dir
		                    (default is false)
		  -h (--help)     : This help message
		  -t (--test)     : Don't actually do anything, just show what would be done
		                    (default is false)
		  -c (--config)   : Specify configuration file. Example file contents:
		                    # Settings
		                    # (you can use globs)
		                    # list of files to link
		                    link='*'
		                    # ignore these files (modifies include)
		                    ignore='Icon* *.md *.sh *.txt scripts'
		                    # just copy these files
		                    copy=''
		                    # create directory itself (not contents), then link the
		                    # children of the directory
		                    link_children='config'
		                    (defaults are in `install.sh`)

		  source dir      : Contains dotfiles. They are expected to NOT have leading
		                    '.' For example, if the dotfile is '.bashrc' then in the
		                    source dir it is 'bashrc'
		                    (default is current directory)
		  destination dir : Where to put symlinks
		                    (default is '~')


Applications
============

## Atom
Install atom, then run this to install packages
    apm install --packages-file packages.txt


To save the list of currently installed packages
    apm list --bare --installed --dev false > packages.txt
