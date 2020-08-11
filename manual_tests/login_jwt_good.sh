#!/bin/sh
echo "`basename "$0"` test:"
host="${1:-localhost}"

curl  --http2-prior-knowledge -c .cookie -b .cookie -i \
    -H "Content-Type: application/json" -X POST -d \
    '{"password":"12345678", "email":"vasya@pupkin.ru"}' \
    http://"$host":8080/login

echo "\n"