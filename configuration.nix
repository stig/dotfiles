{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix search <name>
  environment.systemPackages = with pkgs; [
    awscli
    clj-kondo
    clojure
    clojure-lsp
    coreutils
    curl
    direnv
    ((emacsPackagesFor emacs).emacsWithPackages (epkgs: (with epkgs; [
      ace-window
      cider
      clj-refactor
      clojure-mode
      company
      csv-mode
      diminish
      direnv
      docker
      docker-compose-mode
      dockerfile-mode
      dumb-jump
      edit-indirect
      eglot
      elfeed
      elfeed-org
      emacsql-sqlite
      exec-path-from-shell
      expand-region
      flymake-kondor
      forge
      git-link
      go-mode
      htmlize
      json-mode
      kaocha-runner
      lorem-ipsum
      magit
      markdown-mode
      multiple-cursors
      nix-mode
      nix-sandbox
      nvm
      ol-notmuch
      org-mime
      org-roam
      org-superstar
      orgalist
      ox-gfm
      plantuml-mode
      prescient
      projectile
      protobuf-mode
      rg
      smartparens
      string-inflection
      sudo-edit
      terraform-mode
      verb
      wgrep
      yaml-mode
      yasnippet
    ]) ++ [
      # Notmuch is sensitive to version differences between notmuch.el
      # and the notmuch cli, so it's not recommended to install it
      # from MELPA. This installs the version that ships alongside the
      # notmuch binary.
      notmuch
    ]))
    git
    gnupg
    go
    gopls # go language server
    graphviz
    isync
    jq
    leiningen
    moreutils
    msmtp
    mtr
    nix-direnv
    nodejs
    nodePackages.bash-language-server
    notmuch
    openjdk17
    pinentry
    plantuml
    postgresql
    rabbitmq-server
    redis
    restic
    ripgrep
    rnix-lsp
    shellcheck
    texlive.combined.scheme-full
    yaml-language-server
    yq
  ];

  # Use a custom configuration.nix location.
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

  services.lorri.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
