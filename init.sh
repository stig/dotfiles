exec 2>&1
brew install emacs --with-cocoa --with-gnutls --with-dbus --with-mailutils --with-librsvg --HEAD
:

exec 2>&1
brew install mu --with-emacs
brew install msmtp
brew install isync
brew install coreutils
:

exec 2>&1
mu mkdir ~/Maildir/queue
touch ~/Maildir/queue/.noindex
:

exec 2>&1
brew install aspell
:

exec 2>&1
brew install editorconfig
:

exec 2>&1
brew install the_silver_searcher
:

exec 2>&1
brew install python
:

exec 2>&1
pip install sphinx
:

exec 2>&1
brew cask install graphviz
:

exec 2>&1
brew install trash
:

exec 2>&1
brew install plantuml
:

exec 2>&1
brew install ditaa
:

exec 2>&1
pip install jedi flake8 autopep8
:
