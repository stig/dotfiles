echo "Hello from $0"

notmuch tag --batch <<EOF
+home -- tag:new folder:/home/
+work -- tag:new folder:/work/
+spam -- tag:new folder:/Junk/
+spam -- tag:new folder:/Spam/
EOF
