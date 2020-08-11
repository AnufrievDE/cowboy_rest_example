#!/bin/sh
host="${1:-localhost}"

./registration_good.sh $host
./registration_bad.sh $host
./login_jwt_good.sh $host
./login_good.sh $host
./login_bad_email.sh $host
./login_bad_password.sh $host
./get_user_good.sh $host
./get_user_not_found.sh $host
./get_user_unauthorized.sh $host
./list_users_good.sh $host
./list_users_unauthorized.sh $host
./update_password_good.sh $host
./update_password_unauthorized.sh $host