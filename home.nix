{ pkgs, config, ... }: {
  home.stateVersion = "22.11";

  home.username = "stig";
  home.homeDirectory = "/Users/stig";

  home.packages = with pkgs; [
    (aspellWithDicts (ps: [ps.en]))
    babashka
    bbin
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
    nodePackages.typescript-language-server
    nodePackages.vscode-json-languageserver
    pinentry
    plantuml
    postgresql
    ripgrep
    rnix-lsp
    texlive.combined.scheme-full
    yaml-language-server
    yq
  ];

  home.file.".lein/profiles.clj".source = ./lein-profiles.clj;

  home.file.".config/clojure-lsp/config.edn".source = ./clojure-lsp-config.edn;

  imports = [
    ./emacs/emacs.nix
    ./accounts.email.nix
  ];

  home.sessionVariables = {
    PATH = "$PATH:$HOME/.babashka/bbin/bin";
    GOPATH = "$HOME/.go";
  };

  programs = {
    direnv.enable = true;
    direnv.nix-direnv.enable = true;
    git = {
      enable = true;
      userName = "Stig Brautaset";
      userEmail = "sonar_columns_0n@icloud.com";
      extraConfig = {
        core.pager = "";
        rerere.enabled = true;
        github.user = "stig";
        user.signingkey = "0CC42F1A8D399ACC";
        commit.gpgsign = true;
      };
      ignores = [
        ".DS_Store"
        ".clj-kondo/.cache/"
      ];
    };
    gpg.enable = true;
    home-manager.enable = true;
    zsh.enable = true;
  };

}
