echo "Hello from $0"

notmuch tag --batch <<EOF
+home -- folder:/home/
+work -- folder:/work/
+spam -- folder:/Junk/ or folder:/Spam/
EOF
