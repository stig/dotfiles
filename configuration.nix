{ config, pkgs, ... }:

{
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/.config/nixpkgs/configuration.nix";

  users.users.stig = {
    name = "stig";
    home = "/Users/stig";
  };

  imports = [
    ./agents/mbsync.nix
    ./fonts.nix
    ./settings.nix
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;
  nix.extraOptions = ''
    # dial back garbage collection for direnv
    keep-outputs = true
    keep-derivations = true

    # enable flakes
    experimental-features = nix-command flakes
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  environment.variables = {
    EDITOR = "emacsclient";
  };

  programs.gnupg.agent.enable = true;
  programs.zsh.enable = true;
  
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
