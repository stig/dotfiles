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

  imports = [
    ./accounts.email.nix
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  programs = {
    direnv.enable = true;
    direnv.nix-direnv.enable = true;
    emacs = {
      enable = true;
      package = pkgs.emacs-unstable;
      extraPackages = epkgs: (with epkgs; [
        ace-window
        adoc-mode
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
        jarchive
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
        org-present
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
        visual-fill-column
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
        user.signingkey = "68D77873AEB4B2B9";
        commit.gpgsign = true;
      };
      ignores = [
        ".DS_Store"
        ".clj-kondo/.cache/"
        ".lsp/"
      ];
    };
    gpg.enable = true;
    home-manager.enable = true;
    zsh.enable = true;
  };

}
