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

echo "Delete discarded drafts..."
notmuch search --output=files --format=text0 -- tag:deleted and tag:draft | xargs -0 rm -fv

echo "Delete old discarded messages..."
notmuch search --output=files --format=text0 -- tag:deleted date:..6w | xargs -0 rm -fv
notmuch search --output=files --format=text0 -- tag:spam date:..3w | xargs -0 rm -fv

echo "Move deleted messages out of inbox..."
notmuch search --output=files -- tag:deleted and folder:home/Inbox | mv_without_uid 'home/Deleted Messages'
notmuch search --output=files -- tag:deleted and folder:work/Inbox | mv_without_uid 'work/Deleted Messages'

echo "Move archived messages out of inbox..."
notmuch search --output=files -- tag:archived and folder:home/Inbox | mv_without_uid home/Archive
notmuch search --output=files -- tag:archived and folder:work/Inbox | mv_without_uid 'work/[Gmail]/All Mail'

echo "Move spam messages out of inbox..."
notmuch search --output=files -- tag:spam and folder:home/Inbox | mv_without_uid home/Junk
notmuch search --output=files -- tag:spam and folder:work/Inbox | mv_without_uid 'work/[Gmail]/Spam'

echo "Move non-spam out of spam folder..."
notmuch search --output=files -- not tag:spam and folder:home/Junk | mv_without_uid home/Inbox
notmuch search --output=files -- not tag:spam and folder:'work/[Gmail]/Spam' | mv_without_uid work/Inbox

# Pull new messages from upstream
# Let the hourly job handle updating upstream with local changes
mbsync --all --quiet --pull --new
