# Makohoek's dotfiles

This repository is stored in my home folder using the trick described below:
[https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/]()

## Installation

- Run the install script:
~~~sh
cd ~/ && curl -Lks https://raw.githubusercontent.com/Makohoek/dotfiles/master/setup.sh | /bin/bash
~~~

- Then, initialize the submodules:
~~~sh
git submodule update --init --recursive
~~~
