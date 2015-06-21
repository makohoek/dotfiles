#!/usr/bin/python

from neovim import attach
import os
import sys

# Create a python API session attached to unix domain socket created above:
nvim = attach('socket', path='/tmp/nvim')
current_directory = os.path.expandvars('$PWD') + '/'
nvim.command('cd ' + current_directory)
# initialize fugitive
nvim.command('call fugitive#detect(\''+current_directory+'\')')
nvim.command('Gcommit -s')
