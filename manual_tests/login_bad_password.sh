#!/bin/sh
echo "`basename "$0"` test:"
host="${1:-localhost}"

curl  --http2-prior-knowledge -i \
    -H "Content-Type: application/json" -X POST -d \
    '{"password":"123456789", "email":"vasya@pupkin.ru"}' \
    http://"$host":8080/login

echo "\n"