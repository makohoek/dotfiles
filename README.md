# Makohoek's dotfiles

This repository is stored in my home folder using the trick described below:
[https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/]()

http://thoughtbot.github.io/rcm/rcm.7.html
https://robots.thoughtbot.com/rcm-for-rc-files-in-dotfiles-repos
https://github.com/thoughtbot/rcm#installation

## Installation

- Run the install script:
~~~sh
cd ~/ && curl -Lks https://raw.githubusercontent.com/Makohoek/dotfiles/master/setup.sh | /bin/bash
~~~

- Then, initialize the submodules:
~~~sh
git submodule update --init --recursive
~~~
