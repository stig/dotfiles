# Install Homebrew
which brew || /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install Nix
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume

# Install Nix-Darwin
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
yes | ./result/bin/darwin-installer
