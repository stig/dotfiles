{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix search <name>
  environment.systemPackages = [
    pkgs.aspell
    pkgs.aspellDicts.en
    pkgs.aspellDicts.en-computers
    pkgs.aspellDicts.en-science
    pkgs.awscli
    pkgs.clj-kondo
    pkgs.clojure
    pkgs.curl
    pkgs.direnv
    pkgs.emacs
    pkgs.git
    pkgs.gnupg
    pkgs.isync
    pkgs.jetbrains-mono
    pkgs.jq
    pkgs.leiningen
    pkgs.msmtp
    pkgs.mtr
    pkgs.nix-direnv
    pkgs.notmuch
    pkgs.pinentry
    pkgs.plantuml
    pkgs.postgresql
    pkgs.restic
    pkgs.ripgrep
    pkgs.shellcheck
    pkgs.texlive.combined.scheme-full
  ];
  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  imports = [
    ./fonts.nix
    ./homebrew.nix
    ./mbsync-agent.nix
    ./restic-agent.nix
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
