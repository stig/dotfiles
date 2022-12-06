{
  programs.msmtp = {
    enable = true;

    accounts.default = {
      host = "mail.gandi.net";
      from = "stig@brautaset.org";
      user = "stig@brautaset.org";
    };

    accounts.work = {
      host = "smtp.gmail.com";
      from = "stig@circleci.com";
      user = "stig@circleci.com";
    };
  };
}
