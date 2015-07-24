#!/usr/bin/python

from neovim import attach
import os
import subprocess
import sys

current_directory = os.path.expandvars('$PWD')

if len(sys.argv) > 1:
    file_to_open = sys.argv[1]
    absolute_path_to_file = current_directory + '/' + file_to_open
else:
    absolute_path_to_file = current_directory + '/'

try:
# Create a python API session attached to unix domain socket created above:
    nvim = attach('socket', path='/tmp/nvimm')
    nvim.command('edit ' + absolute_path_to_file)
    nvim.command('cd ' + current_directory)
except Exception, e:
    subprocess.call(['vim', absolute_path_to_file])
