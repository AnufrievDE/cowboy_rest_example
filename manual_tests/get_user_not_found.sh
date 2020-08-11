#!/bin/sh
echo "`basename "$0"` test:"
host="${1:-localhost}"

curl  --http2-prior-knowledge -c .cookie -b .cookie -i \
    -H "Content-Type: application/json" -X GET \
    http://"$host":8080/users/999

echo "\n"