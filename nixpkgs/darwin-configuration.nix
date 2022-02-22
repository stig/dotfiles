{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix search <name>
  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    awscli
    clj-kondo
    clojure
    coreutils
    curl
    direnv
    emacsMacport
    git
    gnupg
    graphviz
    isync
    jq
    leiningen
    moreutils
    msmtp
    mtr
    nix-direnv
    nodejs
    notmuch
    openjdk17
    pinentry
    plantuml
    postgresql
    restic
    ripgrep
    rnix-lsp
    shellcheck
    texlive.combined.scheme-full
    yaml-language-server
    yq
    zprint
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  imports = [
    ./agents/brew.nix
    ./agents/mbsync.nix
    ./agents/restic.nix
    ./fonts.nix
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

  environment.variables = {
    JAVA_HOME = "/run/current-system/sw";
    ASPELL_CONF = "dict-dir ${pkgs.aspellDicts.en}/lib/aspell";

    EDITOR = "emacsclient";

    # For interacting with backup snapshots using restic
    RESTIC_REPOSITORY = "s3:s3.amazonaws.com/brautaset-backups";
    RESTIC_PASSWORD_COMMAND = "security find-generic-password -s restic -w";

    # Use "brew bundle" from anywhere
    HOMEBREW_BUNDLE_FILE = "~/.Brewfile";
  };

  # Notmuch is sensitive to version differences between the emacs
  # package and the cli, so they recommend against installing notmuch
  # from melpa. We can easily use the version that ships with Emacs by
  # linking it into Emacs' site-lisp directory.
  system.activationScripts.postActivation.text = ''
    rm -f ${pkgs.emacsMacport}/share/emacs/site-lisp/notmuch
    ln -s ${pkgs.notmuch.emacs}/share/emacs/site-lisp ${pkgs.emacsMacport}/share/emacs/site-lisp/notmuch
  '';

  # Workaround so Alfred can find Emacs, cf
  # https://github.com/LnL7/nix-darwin/issues/139#issuecomment-748845477
  system.activationScripts.applications.text = pkgs.lib.mkForce (''
    rm -rf ~/Applications/Nix\ Apps
    mkdir -p ~/Applications/Nix\ Apps
    for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
      src="$(/usr/bin/stat -f%Y "$app")"
      cp -r "$src" ~/Applications/Nix\ Apps
    done
  '');

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
