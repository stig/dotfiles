{pkgs, ...}:
{
  # I use Restic to back up to an S3 bucket. I don't want to have to run
  # it manually, so I use this LaunchAgent to schedule backups regularly.
  # To update password, run this:
  # security add-generic-password -a s3.amazonaws.com/brautaset-backups -D 'restic backup password' -s restic -w
  launchd.user.agents.restic = {
    path                            = [ pkgs.restic pkgs.moreutils ];
    environment                     = {
      NIX_SSL_CERT_FILE       = "/etc/ssl/certs/ca-certificates.crt";
      RESTIC_REPOSITORY       = "s3:s3.amazonaws.com/brautaset-backups";
      RESTIC_PASSWORD_COMMAND = "/usr/bin/security find-generic-password -s restic -w";
    };
    script                          = "
restic backup $HOME/.mail $HOME/org $HOME/Sync --exclude $HOME/.mail/.notmuch/xapian --verbose 2>&1 | ts
restic forget --keep-daily 14 --keep-weekly 4 --keep-monthly 12 --keep-yearly 5 --quiet 2>&1 | ts
restic prune --max-unused 10% 2>&1 | ts
";
    serviceConfig.RunAtLoad         = true;
    serviceConfig.StartInterval     = 600;
    serviceConfig.StandardOutPath   = "/Users/stig/Library/Logs/restic/backup.log";
  };

}
