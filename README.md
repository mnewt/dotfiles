Matt's dotfiles
===================

Targeted at [fish shell](http://fishshell.com/)

Older `zsh` dotfiles are still present at this time

Install
-------

[Fork this repo](https://github.com/mnewt/dotfiles) on Github.

Clone your fork (replace `your-github-name` with your Github name).

    git clone git@github.com:your-github-name/dotfiles.git
    cd dotfiles

Run the installer.

    ./install.sh

It creates symlinks for all dotfiles in your home directory. You can safely run
this file multiple times to update.

Included are `fish` dotfiles. To install and switch your shell to `fish` on OS X:

    # Install GNU coreutils, fish, grc, and vimpager
    brew coreutils install fish grc vimpager
    echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
    chsh -s /usr/local/bin/fish

And then run fish

    fish -l
    fish_update_completions

