#!/bin/zsh

ln -vsf $(pwd)/zshrc ~/.zshrc
ln -vsf $(pwd)/aspell.conf ~/.aspell.conf

mkdir -p ~/.config/msmtp
ln -vsf $(pwd)/msmtp/* ~/.config/msmtp

mkdir -p ~/.gnupg
ln -vsf $(pwd)/gnupg/* ~/.gnupg
