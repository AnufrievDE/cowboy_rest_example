#!/bin/sh
echo "`basename "$0"` test:"
host="${1:-localhost}"

curl  --http2-prior-knowledge -c .cookie -b .cookie -i \
    -H "Content-Type: application/json" -X POST -d \
    '{"password":"87654321"}' \
    http://"$host":8080/update_password

echo "\n"