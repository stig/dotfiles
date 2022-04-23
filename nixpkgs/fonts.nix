{pkgs, ...}:
{
  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    jetbrains-mono
    dejavu_fonts
    noto-fonts
    hack-font
  ];
}
