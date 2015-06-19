#!/usr/bin/python

from neovim import attach
import os
import sys

# Create a python API session attached to unix domain socket created above:
nvim = attach('socket', path='/tmp/nvim')
current_directory = os.path.expandvars('$PWD')

if len(sys.argv) > 1:
    file_to_open = sys.argv[1]
    absolute_path_to_file = current_directory + '/' + file_to_open
else:
    absolute_path_to_file = current_directory + '/'

nvim.command('edit ' + absolute_path_to_file)
nvim.command('cd ' + current_directory)
