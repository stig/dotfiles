{ pkgs, config, ... }: {
  home.stateVersion = "22.11";

  home.packages = with pkgs; [
    (aspellWithDicts (ps: [ps.en]))
    awscli
    clj-kondo
    clojure
    clojure-lsp
    coreutils
    curl
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
    go
    gopls # go language server
    graphviz
    isync
    jq
    leiningen
    moreutils
    mtr
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

  accounts.email = {
    accounts.personal = {
      address = "stig@brautaset.org";
      imap.host = "mail.gandi.net";
      maildir.path = "home";
      mbsync = {
        enable = true;
        create = "maildir"; # don't automatically create mailboxes
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;
      };
      msmtp.enable = true;
      passwordCommand = "/usr/bin/security find-generic-password -s mbsync-gandi-password -w";
      primary = true;
      smtp.host = "mail.gandi.net";
      userName = "stig@brautaset.org";
    };

    accounts.work = {
      address = "stig@circleci.com";
      imap.host = "imap.gmail.com";
      maildir.path = "work";
      mbsync = {
        enable = true;
        create = "maildir";
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;
      };
      msmtp.enable = true;
      passwordCommand = "/usr/bin/security find-generic-password -s mbsync-gmail-password -w";
      smtp.host = "smtp.gmail.com";
      userName = "stig@circleci.com";
    };
  };

  home.file."${config.xdg.dataHome}/gnupg/.gpg-agent.conf".text = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
  '';

  programs = {
    direnv.enable = true;
    direnv.nix-direnv.enable = true;
    git = {
      enable = true;
      userName = "Stig Brautaset";
      userEmail = "stig@brautaset.org";
      extraConfig = {
        core.pager = "";
        rerere.enabled = true;
      };
      ignores = [
        ".DS_Store"
        ".clj-kondo/.cache/"
        ".lsp/"
      ];
    };
    gpg = {
      enable = true;
      homedir = "${config.xdg.dataHome}/gnupg";
    };
    home-manager.enable = true;
    mbsync.enable = true;
    msmtp.enable = true;
    zsh.enable = true;
  };

}
