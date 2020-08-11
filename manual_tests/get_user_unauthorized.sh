#!/bin/sh
echo "`basename "$0"` test:"
host="${1:-localhost}"

curl  --http2-prior-knowledge -i \
    -H "Content-Type: application/json" -X GET \
    http://"$host":8080/users/1

echo "\n"