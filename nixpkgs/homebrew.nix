{
  homebrew.enable = true;
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [
    "homebrew/cask"
  ];

  homebrew.casks = [
    "1password"
    "alfred"
    "docker"
    "fantastical"
    "meetingbar"
    "minecraft"
    "refined-github-safari"
    "signal"
    "slack"
    "spotify"
    "syncthing"
    "zoom"
  ];
}
