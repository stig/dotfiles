{ pkgs, ... }: {
  home.stateVersion = "22.11";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
  ];
}
