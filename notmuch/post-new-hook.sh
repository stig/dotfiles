echo "Hello from $0"

notmuch tag --batch <<EOF
+personal -- tag:unread folder:/home/
+work -- tag:unread folder:/work/
+spam -- folder:/Spam/
EOF
