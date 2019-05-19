/*
This is a nix expression to build Emacs and some Emacs packages I like
from source on any distribution where Nix is installed. This will install
all the dependencies from the nixpkgs repository and build the binary files
without interfering with the host distribution.

To build the project, type the following from the current directory:

$ nix-build emacs.nix

To run the newly compiled executable:

$ ./result/bin/emacs
*/
{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [

    ace-window
    aggressive-indent
    amx
    cider
    clj-refactor
    clojure-mode
    company
    counsel
    counsel-projectile
    editorconfig
    elfeed
    elpy
    ensime
    exec-path-from-shell
    gist
    git-auto-commit-mode
    git-link
    graphviz-dot-mode
    markdown-mode
    material-theme
    multiple-cursors
    nix-mode
    projectile
    sbt-mode
    scala-mode
    smartparens
    string-inflection
    swiper
    use-package
    which-key
    writegood-mode
    yaml-mode

  ]) ++ (with epkgs.melpaPackages; [

    ag
    ivy
    ivy-hydra
    nix-sandbox
    ox-jira
    pipenv
    sphinx-frontend
    wgrep
    wgrep-ag

  ]) ++ (with epkgs.elpaPackages; [

    beacon         # ; highlight my cursor when scrolling
    orgalist

  ]) ++ (with epkgs.orgPackages; [

    org-plus-contrib

  ]) ++ [

    pkgs.aspell
    pkgs.aspellDicts.en
    pkgs.awscli
    pkgs.clojure
    pkgs.curl
    pkgs.ditaa
    pkgs.editorconfig-core-c
    pkgs.fluidsynth
    pkgs.freepats
    pkgs.gnused
    pkgs.gnutls
    pkgs.graphviz
    pkgs.isync
    pkgs.jq
    pkgs.msmtp
    pkgs.notmuch
    pkgs.pinentry
    pkgs.plantuml
    pkgs.pwgen
    pkgs.python3
    pkgs.sbt
    pkgs.silver-searcher

  ])
