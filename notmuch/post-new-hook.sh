echo "Hello from $0"

notmuch tag --batch <<EOF
+personal -- tag:unread folder:/icloud/
+work -- tag:unread folder:/work/
+spam -- folder:/Spam/
-inbox -- tag:sent and tag:inbox
EOF
