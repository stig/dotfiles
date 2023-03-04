echo "Hello from $0"

notmuch tag --batch <<EOF
+personal -- tag:unread folder:/home/
+personal -- tag:unread folder:/icloud/
+work -- tag:unread folder:/work/
+spam -- folder:/Spam/
EOF
