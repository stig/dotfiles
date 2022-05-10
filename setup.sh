#!/bin/zsh

case $(hostname) in
    cci-stig-*)
	MBSYNC_CONFIG=$(pwd)/mbsync/work
	;;
    Margil.local)
	MBSYNC_CONFIG=$(pwd)/mbsync/personal
	;;
    *)
	echo "Where am I running?"
	exit 1
	;;
esac

ln -vsf $(pwd)/direnvrc ~/.direnvrc
ln -vsf $(pwd)/Brewfile ~/.Brewfile

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

ln -vsf "$MBSYNC_CONFIG" ~/.mbsyncrc

which brew || ./install-brew.sh
which mas || brew install mas
which nix-env || ./install-nix.sh --darwin-use-unencrypted-nix-store-volume
which darwin-rebuild || {
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    (echo n ; yes) | ./result/bin/darwin-installer
}
