#!/bin/zsh

ln -vsf $(pwd)/zshrc ~/.zshrc

mkdir -p ~/.config/msmtp
ln -vsf $(pwd)/msmtp/* ~/.config/msmtp
