{
  homebrew.enable = true;
  homebrew.global.brewfile = true;
  homebrew.global.noLock = true;

  homebrew.taps = [
    "homebrew/cask"
  ];

  homebrew.casks = [
    "alfred"
    "dash"
    "discord"
    "docker"
    "minecraft"
    "signal"
    "spotify"
    "syncthing"
    "zoom"
  ];

  homebrew.masApps = {
    "1Password" = 1333542190;
    Fantastical = 975937182;
    Horo = 1437226581;
    Kindle = 405399194;
    MeetingBar = 1532419400;
    NextDNS = 1464122853;
    Pocket = 568494494;
    "Refined GitHub" = 1519867270;
    "Save to Pocket" = 1477385213;
    "Speedtest by Ookla" = 1153157709;
    Slack = 803453959;
    WhatsApp = 1147396723;
    Xcode = 497799835;
  };
}
