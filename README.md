This is a dotfile repository!
=====================================

There are two ways of installing these dotfiles.

1. Manual

```bash
git clone https://github.com/akagr/dotfiles ~/dotfiles
ln -s ~/dotfiles/.vimrc ~/.vimrc
ln -s ~/dotfiles/.vim ~/.vim
ln -s ~/dotfiles/.tmux.conf ~/.tmux.conf
ln -s ~/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/dotfiles/.irssi ~/.irssi
```

2. Use the setup script. It will clone the repo, symlink vim specific files and clone the neobundle plugin too.

```bash
curl https://raw.githubusercontent.com/akagr/dotfiles/master/setup.sh | sh
```
