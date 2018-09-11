# Makohoek's dotfiles

This repository is stored in my home folder using the trick described below:
[https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/]()

http://thoughtbot.github.io/rcm/rcm.7.html
https://robots.thoughtbot.com/rcm-for-rc-files-in-dotfiles-repos
https://github.com/thoughtbot/rcm#installation

## Installation

- Clone the repository:
```sh
git clone https://github.com/Makohoek/dotfiles.git ~/.dotfiles
```

- Then, initialize the submodules:
```sh
cd ~/.dotfiles && git submodule update --init --recursive
```

- Then, use `rcm` to create the symlinks:
```sh
cd && rcup -v
```
