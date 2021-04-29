export EDITOR=emacsclient

# Emacs can't find pdflatex without this, for some reason. This
# *should* be handled by system setup, but I don't have time to debug
# why it's not working at the moment. I hope to look into it again
# after the upgrade to Big Sur.
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

# For interacting with backup snapshots using restic
export RESTIC_REPOSITORY="s3:s3.amazonaws.com/brautaset-backups"
export RESTIC_PASSWORD_COMMAND="security find-generic-password -s restic -w"

# Don't auto-update brew
export HOMEBREW_NO_AUTO_UPDATE=1
