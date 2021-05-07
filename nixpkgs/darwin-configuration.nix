{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix search <name>
  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    awscli
    clj-kondo
    clojure
    curl
    direnv
    emacs
    git
    gnupg
    isync
    jq
    leiningen
    moreutils
    msmtp
    mtr
    nix-direnv
    notmuch
    pinentry
    plantuml
    postgresql
    restic
    ripgrep
    shellcheck
    texlive.combined.scheme-full
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  imports = [
    ./agents/brew.nix
    ./agents/mbsync.nix
    ./agents/restic.nix
    ./fonts.nix
    ./homebrew.nix
    ./settings.nix
  ];

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # This is for direnv, to dial back garbage collection
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
