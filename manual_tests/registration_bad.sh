#!/bin/sh
echo "`basename "$0"` test:"
host="${1:-localhost}"

curl  --http2-prior-knowledge -c .cookie -b .cookie -i \
    -H "Content-Type: application/json" -X POST -d \
    '{"fname":"Va","lname":"Pu","password":"1234567", "email":"vasyapupkin.ru"}' \
    http://"$host":8080/register

echo "\n"