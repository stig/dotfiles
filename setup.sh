#!/bin/zsh

ln -vsf $(pwd)/zshrc ~/.zshrc
ln -vsf $(pwd)/aspell.conf ~/.aspell.conf
ln -vsf $(pwd)/direnvrc ~/.direnvrc

mkdir -p ~/.config/msmtp
ln -vsf $(pwd)/msmtp/* ~/.config/msmtp

mkdir -p ~/.gnupg
ln -vsf $(pwd)/gnupg/* ~/.gnupg

mkdir -p ~/.nixpkgs
ln -vsf $(pwd)/nixpkgs/* ~/.nixpkgs

which brew || ./install-brew.sh
