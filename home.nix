{ pkgs, config, ... }: {
  home.stateVersion = "22.11";

  home.username = "stig";
  home.homeDirectory = "/Users/stig";

  home.packages = with pkgs; [
    (aspellWithDicts (ps: [ps.en]))
    clj-kondo
    clojure
    clojure-lsp
    coreutils
    curl
    go # required for eglot-go to follow installed libraries.
    gopls # go language server -- also required for eglot-go.
    graphviz
    isync
    jq
    leiningen
    moreutils
    nodejs # for Alfred Jira Plugin
    nodePackages.bash-language-server
    pinentry
    plantuml
    postgresql
    ripgrep
    rnix-lsp
    texlive.combined.scheme-full
    yaml-language-server
    yq
  ];

  home.file.".mailcap".text = ''
    image/*; open %s
    application/pdf; open %s
  '';

  home.file.".lein/profiles.clj".source = ./lein-profiles.clj;

  accounts.email = {
    accounts.personal = {
      address = "stig@brautaset.org";
      imap.host = "mail.gandi.net";
      maildir.path = "home";
      mbsync = {
        enable = true;
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;

        groups.personal.channels.inbox.farPattern = "";
        groups.personal.channels.inbox.nearPattern = "";

        groups.personal.channels.archive.farPattern = "Archive";
        groups.personal.channels.archive.nearPattern = "Archive";

        groups.personal.channels.sent.farPattern = "Sent Messages";
        groups.personal.channels.sent.nearPattern = "Sent";

        groups.personal.channels.junk.farPattern = "Junk";
        groups.personal.channels.junk.nearPattern = "Spam";

        groups.personal.channels.trash.farPattern = "Deleted Messages";
        groups.personal.channels.trash.nearPattern = "Trash";
      };
      msmtp.enable = true;
      notmuch.enable = true;
      realName = "Stig Brautaset";
      passwordCommand = "/usr/bin/security find-generic-password -s mbsync-gandi-password -w";
      primary = true;
      smtp.host = "mail.gandi.net";
      userName = "stig@brautaset.org";
    };

    accounts.icloud = {
      address = "sonar_columns_0n@icloud.com";
      imap.host = "imap.mail.me.com";
      maildir.path = "icloud";
      mbsync = {
        enable = true;
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;

        groups.icloud.channels.inbox.farPattern = "";
        groups.icloud.channels.inbox.nearPattern = "";

        groups.icloud.channels.archive.farPattern = "Archive";
        groups.icloud.channels.archive.nearPattern = "Archive";

        groups.icloud.channels.sent.farPattern = "Sent Messages";
        groups.icloud.channels.sent.nearPattern = "Sent";

        groups.icloud.channels.junk.farPattern = "Junk";
        groups.icloud.channels.junk.nearPattern = "Spam";

        groups.icloud.channels.trash.farPattern = "Deleted Messages";
        groups.icloud.channels.trash.nearPattern = "Trash";
      };
      msmtp.enable = true;
      smtp.port = 587;
      smtp.tls.useStartTls = true;
      notmuch.enable = true;
      realName = "Stig Brautaset";
      passwordCommand = "/usr/bin/security find-internet-password -s imap.mail.me.com -w";
      smtp.host = "smtp.mail.me.com";
      userName = "sonar_columns_0n@icloud.com";
    };

    accounts.work = {
      address = "stig@circleci.com";
      imap.host = "imap.gmail.com";
      maildir.path = "work";
      mbsync = {
        enable = true;
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;

        groups.work.channels.inbox.farPattern = "";
        groups.work.channels.inbox.nearPattern = "";

        groups.work.channels.archive.farPattern = "[Gmail]/All Mail";
        groups.work.channels.archive.nearPattern = "Archive";

        groups.work.channels.sent.farPattern = "[Gmail]/Sent Mail";
        groups.work.channels.sent.nearPattern = "Sent";

        groups.work.channels.spam.farPattern = "[Gmail]/Spam";
        groups.work.channels.spam.nearPattern = "Spam";

        groups.work.channels.trash.farPattern = "[Gmail]/Trash";
        groups.work.channels.trash.nearPattern = "Trash";

      };
      msmtp.enable = true;
      notmuch.enable = true;
      passwordCommand = "/usr/bin/security find-generic-password -s mbsync-gmail-password -w";
      smtp.host = "smtp.gmail.com";
      userName = "stig@circleci.com";
    };
  };

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  programs = {
    direnv.enable = true;
    direnv.nix-direnv.enable = true;
    emacs = {
      enable = true;
      extraPackages = epkgs: (with epkgs; [
        ace-window
        cider
        clj-refactor
        clojure-mode
        company
        consult
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
        marginalia
        markdown-mode
        multiple-cursors
        nix-mode
        nix-sandbox
        ol-notmuch
        orderless
        org-mime
        org-roam
        org-superstar
        orgalist
        ox-gfm
        plantuml-mode
        projectile
        protobuf-mode
        rg
        smartparens
        string-inflection
        sudo-edit
        terraform-mode
        verb
        vertico
        wgrep
        yaml-mode
        yasnippet
      ]);
    };
    git = {
      enable = true;
      userName = "Stig Brautaset";
      userEmail = "stig@brautaset.org";
      extraConfig = {
        core.pager = "";
        rerere.enabled = true;
        github.user = "stig";
      };
      ignores = [
        ".DS_Store"
        ".clj-kondo/.cache/"
        ".lsp/"
      ];
    };
    gpg.enable = true;
    home-manager.enable = true;
    mbsync.enable = true;
    mbsync.extraConfig = ''
      SyncState *
    '';
    msmtp.enable = true;
    notmuch = {
      enable = true;
      new = {
        ignore = [
          ".DS_Store"
          ".isyncuidmap.db"
          ".mbsyncstate"
          ".mbsyncstate.journal"
          ".mbsyncstate.lock"
          ".mbsyncstate.new"
          ".uidvalidity"
        ];
      };
      hooks = {
        preNew = (builtins.readFile ./notmuch/pre-new-hook.sh);
        postNew = (builtins.readFile ./notmuch/post-new-hook.sh);
      };
    };
    zsh.enable = true;
  };

}
