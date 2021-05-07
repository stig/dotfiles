{pkgs, ...}:
{
  launchd.user.agents.brew-update = {
    path                            = [ pkgs.moreutils # for the ts utility
                                        "/usr/bin"
                                        "/usr/local/bin" ];
    environment                     = {
      NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    };
    command                         = "brew update 2>&1 | ts";
    serviceConfig.RunAtLoad         = true;
    serviceConfig.StartInterval     = 86400; # once a day
    serviceConfig.StandardOutPath   = "/Users/stig/Library/Logs/Homebrew/brew-update.log";
  };
}
