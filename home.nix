{ pkgs, config, ... }: {
  home.stateVersion = "22.11";

  home.packages = with pkgs; [
    (aspellWithDicts (ps: [ps.en]))
    clj-kondo
    clojure
    clojure-lsp
    coreutils
    curl
    go
    gopls # go language server
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
      };
      msmtp.enable = true;
      notmuch.enable = true;
      realName = "Stig Brautaset";
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
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;
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
    msmtp.enable = true;
    notmuch = {
      enable = true;
      new = {
        tags = ["new"];
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
