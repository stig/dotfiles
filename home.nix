{ pkgs, ... }: {
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

  programs = {
    direnv.enable = true;
    direnv.nix-direnv.enable = true;
    home-manager.enable = true;
    zsh.enable = true;
  };

}
