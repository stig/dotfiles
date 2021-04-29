#!/bin/zsh

ln -vsf $(pwd)/zshenv ~/.zshenv
ln -vsf $(pwd)/aspell.conf ~/.aspell.conf
ln -vsf $(pwd)/direnvrc ~/.direnvrc

mkdir -p ~/.config/msmtp
ln -vsf $(pwd)/msmtp/* ~/.config/msmtp

mkdir -p ~/.gnupg
ln -vsf $(pwd)/gnupg/* ~/.gnupg

mkdir -p ~/.nixpkgs
ln -vsf $(pwd)/nixpkgs/* ~/.nixpkgs

which brew || ./install-brew.sh
which mas || brew install mas
which nix-env || ./install-nix.sh --darwin-use-unencrypted-nix-store-volume
which darwin-rebuild || {
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    (echo n ; yes) | ./result/bin/darwin-installer
}
