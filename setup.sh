#!/bin/zsh

ln -vsf $(pwd)/direnvrc ~/.direnvrc

mkdir -p ~/.config/emacs/straight
ln -vsf $(pwd)/emacs/straight/* ~/.config/emacs/straight

mkdir -p ~/.config/msmtp
ln -vsf $(pwd)/msmtp/* ~/.config/msmtp

mkdir -p ~/.gnupg
ln -vsf $(pwd)/gnupg/* ~/.gnupg

if [ -L ~/.nixpkgs ] ; then
    rm ~/.nixpkgs
elif [ -e ~/.nixpkgs ] ; then
    echo "~/.nixpkgs exists but is not a symlink"
    exit 1
fi

ln -vsf $(pwd)/nixpkgs ~/.nixpkgs

(cd $(pwd)/mbsync ; cat personal work > ~/.mbsyncrc)

which nix-env || ./install-nix.sh --darwin-use-unencrypted-nix-store-volume
which darwin-rebuild || {
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    (echo n ; yes) | ./result/bin/darwin-installer
}
