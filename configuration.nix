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
    ./agents/restic.nix
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
    JAVA_HOME = "/run/current-system/sw";

    EDITOR = "emacsclient";

    # For interacting with backup snapshots using restic
    RESTIC_REPOSITORY = "s3:s3.amazonaws.com/brautaset-backups";
    RESTIC_PASSWORD_COMMAND = "security find-generic-password -s restic -w";
  };

  programs.zsh.enable = true;  # default shell on catalina
  
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
