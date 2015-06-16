#!/usr/bin/python

from neovim import attach
import os
import sys

# Create a python API session attached to unix domain socket created above:
nvim = attach('socket', path='/tmp/nvim')
current_directory = os.path.expandvars('$PWD')

if len(sys.argv) > 1:
    file_to_open = sys.argv[1]
    vsplit_arguments = current_directory + '/' + file_to_open
else:
    vsplit_arguments = current_directory + '/'

nvim.command('vsplit ' + vsplit_arguments)
nvim.command('cd ' + current_directory)
