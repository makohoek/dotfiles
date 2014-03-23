dotfiles
========

This is a backup of the dotfiles i use the most.
Feel free to copycat anything you want!

## Installation
### submodules
After cloning the repository, we  must initialize the
submodules:

    git submodule update --init --recursive

### symbolic links
go to your home folder
I use *symbolic links* to point towards the files i need.

For example, if you want to use the .vim directory, do

    ln -s /path/to/dotfiles.git/.vim


