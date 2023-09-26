echo "Hello from $0"

set -euo pipefail
IFS=$'\n\t'

# Update paths to messages that might have moved
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
notmuch search --output=files -- tag:deleted and folder:icloud/Inbox | mv_without_uid icloud/Trash
notmuch search --output=files -- tag:deleted and folder:work/Inbox | mv_without_uid work/Trash

echo "Move spam messages out of inbox..."
notmuch search --output=files -- tag:spam and folder:icloud/Inbox | mv_without_uid icloud/Spam
notmuch search --output=files -- tag:spam and folder:work/Inbox | mv_without_uid work/Spam

echo "Move archived messages out of inbox..."
notmuch search --output=files -- -tag:inbox and folder:icloud/Inbox | mv_without_uid icloud/Archive
notmuch search --output=files -- -tag:inbox and folder:work/Inbox | mv_without_uid work/Archive

echo "Move non-spam out of spam folder..."
notmuch search --output=files -- not tag:spam and folder:icloud/Spam | mv_without_uid icloud/Inbox
notmuch search --output=files -- not tag:spam and folder:work/Spam | mv_without_uid work/Inbox
