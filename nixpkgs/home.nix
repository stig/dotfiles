{ pkgs, ... }: {
  home.username = "stig";
  home.homeDirectory = "/Users/stig";
  home.stateVersion = "22.11";
  programs.home-manager.enable = true;
}
