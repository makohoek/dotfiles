# Makohoek's dotfiles

:dragon: here be dragons :dragon:

## Prerequisite
My dotfiles are managed through `rcm`.
To install `rcm`, visit https://github.com/thoughtbot/rcm#installation


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
cd && env RCRC=$HOME/.dotfiles/rcrc rcup
```

## More information on `rcm`
- http://thoughtbot.github.io/rcm/rcm.7.html
- https://robots.thoughtbot.com/rcm-for-rc-files-in-dotfiles-repos
