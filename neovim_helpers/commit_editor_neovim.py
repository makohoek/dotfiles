#!/usr/bin/python
# This is intended to be run within a neovim instance.
# Configure git to set this script as commit editor:
#     git config core.editor ~/dotfiles/neovim_helpers/run_fugitive_commit.py

from neovim import attach
import os
import sys
import time

def is_buffer_name_in_list(nvim_instance, buffername):
    is_buffer=lambda buf: buf.valid and buf.name == buffername
    return filter(is_buffer, nvim_instance.buffers)  != []

if __name__ == '__main__':
    # Create a python API session attached to unix domain socket created above:
    nvim = attach('socket', path='/tmp/nvim')
    # Set pwd in buffer to the same as the shell we are invoked from
    current_directory = os.path.expandvars('$PWD') + '/'
    nvim.command('cd ' + current_directory)
    # edit the commit message file
    commit_message_filepath=sys.argv[1]
    nvim.command('edit '+commit_message_filepath)
    # when hiding the buffer, it will be deleted from the buffer list.
    # deleting the buffer from the buffer list will tell git that we are done
    # writing the commit message
    nvim.command('setlocal bufhidden=wipe')
    # ugly logic to verify that the commit_messsage_buffer is still present in
    # buffer list
    while is_buffer_name_in_list(nvim, commit_message_filepath):
        time.sleep(0.1)
