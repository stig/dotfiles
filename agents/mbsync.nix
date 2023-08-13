{pkgs, ...}:
{
  launchd.user.agents.mbsync = {
    path = [pkgs.isync pkgs.moreutils];

    command = "mbsync --all --verbose | ts";

    serviceConfig = {
      RunAtLoad = true;
      StartInterval = 600;
      StandardErrorPath = "/Users/stig/Library/Logs/mbsync/stderr.log";
      StandardOutPath = "/Users/stig/Library/Logs/mbsync/stdout.log";
    };
  };
}
