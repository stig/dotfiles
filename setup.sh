#!/bin/zsh

if [ -L ~/.config/nixpkgs ] ; then
    rm ~/.config/nixpkgs
elif [ -e ~/.config/nixpkgs ] ; then
    echo "~/.config/nixpkgs exists but is not a symlink"
    exit 1
fi
ln -vsf $(pwd) ~/.config/nixpkgs

(cd $(pwd)/mbsync ; cat personal work > ~/.mbsyncrc)

which nix-env || ./install-nix.sh --darwin-use-unencrypted-nix-store-volume
which darwin-rebuild || {
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    (echo n ; yes) | ./result/bin/darwin-installer
}

echo "now run:"
echo "darwin-rebuild switch --flake ."
