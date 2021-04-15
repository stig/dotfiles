{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix search <name>
  environment.systemPackages =
    [
      pkgs.aspell
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
      pkgs.texlive.combined.scheme-full
    ];
  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    jetbrains-mono
    dejavu_fonts
    noto-fonts
    hack-font
  ];

  launchd.user.agents.mbsync = {
    path                            = [ pkgs.isync ];
    environment                     = {
      NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    };
    command                         = "mbsync -aqV";
    serviceConfig.RunAtLoad         = true;
    serviceConfig.StartInterval     = 3600;
    serviceConfig.StandardErrorPath = "/Users/stig/Library/Logs/mbsync/stderr.log";
    serviceConfig.StandardOutPath   = "/Users/stig/Library/Logs/mbsync/stdout.log";
  };


  # I use Restic to back up to an S3 bucket. I don't want to have to run
  # it manually, so I use this LaunchAgent to schedule backups regularly.
  # To update password, run this:
  # security add-generic-password -a s3.amazonaws.com/brautaset-backups -D 'restic backup password' -s restic -w

  launchd.user.agents.restic = {
    path                            = [ pkgs.restic ];
    environment                     = {
      NIX_SSL_CERT_FILE       = "/etc/ssl/certs/ca-certificates.crt";
      RESTIC_REPOSITORY       = "s3:s3.amazonaws.com/brautaset-backups";
      RESTIC_PASSWORD_COMMAND = "/usr/bin/security find-generic-password -s restic -w";
    };
    command                         = "restic backup $HOME/.mail $HOME/org $HOME/Sync --exclude $HOME/.mail/.notmuch/xapian --verbose";
    serviceConfig.RunAtLoad         = true;
    serviceConfig.StartInterval     = 600;
    serviceConfig.StandardErrorPath = "/Users/stig/Library/Logs/restic/stderr.log";
    serviceConfig.StandardOutPath   = "/Users/stig/Library/Logs/restic/stdout.log";
  };

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
