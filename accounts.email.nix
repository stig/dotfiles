{...}:
{
  accounts.email = {
    accounts.personal = {
      address = "stig@brautaset.org";
      notmuch.enable = true;
      realName = "Stig Brautaset";
    };

    accounts.icloud = {
      address = "sonar_columns_0n@icloud.com";
      imap.host = "imap.mail.me.com";
      maildir.path = "icloud";
      mbsync = {
        enable = true;
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;

        groups.icloud.channels.inbox.farPattern = "";
        groups.icloud.channels.inbox.nearPattern = "";

        groups.icloud.channels.archive.farPattern = "Archive";
        groups.icloud.channels.archive.nearPattern = "Archive";

        groups.icloud.channels.sent.farPattern = "Sent Messages";
        groups.icloud.channels.sent.nearPattern = "Sent";

        groups.icloud.channels.junk.farPattern = "Junk";
        groups.icloud.channels.junk.nearPattern = "Spam";

        groups.icloud.channels.trash.farPattern = "Deleted Messages";
        groups.icloud.channels.trash.nearPattern = "Trash";
      };
      msmtp.enable = true;
      smtp.port = 587;
      smtp.tls.useStartTls = true;
      notmuch.enable = true;
      realName = "Stig Brautaset";
      passwordCommand = "/usr/bin/security find-internet-password -s imap.mail.me.com -w";
      primary = true;
      smtp.host = "smtp.mail.me.com";
      userName = "sonar_columns_0n@icloud.com";
    };

    accounts.work = {
      address = "stig@circleci.com";
      imap.host = "imap.gmail.com";
      maildir.path = "work";
      mbsync = {
        enable = true;
        expunge = "both";
        extraConfig.account.PipelineDepth = 10;
        extraConfig.account.Timeout = 60;

        groups.work.channels.inbox.farPattern = "";
        groups.work.channels.inbox.nearPattern = "";

        groups.work.channels.archive.farPattern = "[Gmail]/All Mail";
        groups.work.channels.archive.nearPattern = "Archive";

        groups.work.channels.sent.farPattern = "[Gmail]/Sent Mail";
        groups.work.channels.sent.nearPattern = "Sent";

        groups.work.channels.spam.farPattern = "[Gmail]/Spam";
        groups.work.channels.spam.nearPattern = "Spam";

        groups.work.channels.trash.farPattern = "[Gmail]/Trash";
        groups.work.channels.trash.nearPattern = "Trash";

      };
      msmtp.enable = true;
      notmuch.enable = true;
      passwordCommand = "/usr/bin/security find-generic-password -s mbsync-gmail-password -w";
      smtp.host = "smtp.gmail.com";
      userName = "stig@circleci.com";
    };
  };

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch.enable = true;

  programs.mbsync.extraConfig = ''
      SyncState *
  '';

  programs.notmuch.new.ignore = [
    ".DS_Store"
    ".isyncuidmap.db"
    ".mbsyncstate"
    ".mbsyncstate.journal"
    ".mbsyncstate.lock"
    ".mbsyncstate.new"
    ".uidvalidity"
  ];

  programs.notmuch.hooks.preNew = (builtins.readFile ./notmuch/pre-new-hook.sh);
  programs.notmuch.hooks.postNew = (builtins.readFile ./notmuch/post-new-hook.sh);
}
