{ pkgs, ... }:
{
  home.file.".config/emacs/init.el".source = ./init.el;
  home.file.".config/emacs/early-init.el".source = ./early-init.el;

  home.sessionVariables = {
    EDITOR = "emacsclient";
  };

  programs.emacs = {
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
      eglot-java
      elfeed
      elfeed-org
      exec-path-from-shell
      expand-region
      flymake-kondor
      forge
      git-auto-commit-mode
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
      org-roam-ui
      org-superstar
      orgalist
      outshine
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
      which-key
      yaml-mode
      yasnippet
    ]);
  };


  # This lets me open attachments from Notmuch more conveniently.
  home.file.".mailcap".text = ''
    image/*; open %s
    application/pdf; open %s
  '';

}
