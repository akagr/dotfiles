#!/bin/bash

# Akash Agrawal akashagrawal.me akagr[dot]outlook[dot]com
# This is a setup script for downloading my dotfiles and setting up vim config automatically

# If destination not supplied, use default destination.
# If destination supplied but not present, raise error.
DOTFILES=$1
if [ -z $DOTFILES ]; then
  DOTFILES="${HOME}/dotfiles"
  mkdir -p $DOTFILES
elif [ ! -d $DOTFILES ]; then
  echo "Directory $DOTFILES does not exists."
  exit 1
fi

# If git not present, raise error.
if [ -z $(command -v git) ]; then
  echo ""
  echo "Git not found. Run one of the following commands to install git:"
  echo ""
  echo "sudo apt-get install git (Ubuntu, Debian, Mint)"
  echo "sudo yum install git (Fedora, Red-Hat, CentOS)"
  echo "sudo pacman -S git (Arch)"
  exit 1
fi

# Clone dotfiles
git clone "https://github.com/akagr/dotfiles.git" $DOTFILES

echo ""
echo "Setting up vimrc and plugins"

# Create symlinks for vimrc and .vim
ln -s "${DOTFILES}/.vimrc" ~/.vimrc
ln -s "${DOTFILES}/.vim" ~/.vim

# Clone Neobundle for installing vim plugins
git clone https://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim

echo ""
echo "Completed :) Don't forget to run NeobundleInstall from vim."
