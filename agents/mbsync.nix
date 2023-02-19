{pkgs, ...}:
{
  launchd.user.agents.mbsync = {
    path                            = [ pkgs.isync ];
    environment                     = {
      NIX_SSL_CERT_FILE = "/etc/ssl/certs/ca-certificates.crt";
    };
    command                         = "mbsync --all --quiet --verbose";
    serviceConfig.RunAtLoad         = true;
    serviceConfig.StartInterval     = 3600;
    serviceConfig.StandardErrorPath = "/Users/stig/Library/Logs/mbsync/stderr.log";
    serviceConfig.StandardOutPath   = "/Users/stig/Library/Logs/mbsync/stdout.log";
  };
}
