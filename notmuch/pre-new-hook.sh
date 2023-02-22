echo "Hello from $0"

set -euo pipefail
IFS=$'\n\t'

# Run first to ensure statistics are up to date
notmuch new --no-hooks

# Strip UIDs from filenames when moving mails so that mbsync doesn't
# get confused.
mv_without_uid () {
    while IFS= read -r name; do
        new_name="$(basename $name | awk -F ',' '{print $1}')"
        [ -f "$name" ] && mv -nv "$name" "/Users/stig/Maildir/$1/new/$new_name" | sed 's,/Users/stig/Maildir/,,g'
    done
}

# Archive messages
notmuch search --output=files -- tag:archived and folder:home/Inbox | mv_without_uid home/Archive
notmuch search --output=files -- tag:archived and folder:work/Inbox | mv_without_uid 'work/[Gmail]/All Mail'

# Thou shalt not suffer a deleted draft to live
notmuch search --output=files --format=text0 -- tag:deleted and tag:draft | xargs -0 echo rm -fv

# Delete old deleted and spam messages
notmuch search --output=files --format=text0 -- tag:deleted date:..6w | xargs -0 rm -fv
notmuch search --output=files --format=text0 -- tag:spam date:..3w | xargs -0 rm -fv

# Move deleted messages
notmuch search --output=files -- tag:home and tag:deleted and not folder:/Deleted.Messages/ | mv_without_uid 'home/Deleted Messages'
notmuch search --output=files -- tag:work and tag:deleted and not folder:/Deleted.Messages/ | mv_without_uid 'work/Deleted Messages'

# Move spam messages
notmuch search --output=files -- tag:home and tag:spam and not folder:/Junk/ | mv_without_uid home/Junk
notmuch search --output=files -- tag:work and tag:spam and not folder:/Spam/ | mv_without_uid 'work/[Gmail]/Spam'

# Move incorrectly-tagged spam OUT of Spam folder
notmuch search --output=files -- not tag:spam and folder:/Junk/ | mv_without_uid home/Inbox
notmuch search --output=files -- not tag:spam and folder:/Spam/ | mv_without_uid work/Inbox

# Remove 'new' tag for old messages
notmuch tag -new -- tag:new

# Pull new messages from upstream
# Let the hourly job handle updating upstream with local changes
mbsync --all --quiet --pull --new
