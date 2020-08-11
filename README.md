# Cowboy Rest Server example

This is an erlang application created to show up a way of use of the cowboy web server.

It allows to:
1. Register user on a server from json:  
    `http://localhost:8080/register`
2. Authenticate user from JSON:  
    `http://localhost:8080/login`
3. Authorize user from [JWT](https://jwt.io)(from cookie), for all the methods below.
4. List users:  
    `http://localhost:8080/users`
5. Get user (retrieving id from url):  
    `http://localhost:8080/user/1`
6. Update password from json:  
    `http://localhost:8080/update_password`

## Configuration parameters (with default values):
  * `cowboy_host` - web-server host;
  * `cowboy_port` - web-server port;
  * `secret` - secret for JWT generation;
  * `pools` - several options for managing pool of connections to mariadb

## Docker-compose run
  1. check .env, sys.config.src file to set needed configuration parameters
  2. run:
     * `docker-compose up --build --force-recreate --no-deps`
  3. or build and run:
     * `docker-compose build`
     * `docker-compose up`

## Local run
1. check sys.config file to set needed configuration parameters;
2. make sure you have mariadb running on configured host/port;
3. run ./sql/init_db.sql script in appropriate db for tables initialization;
4. run
   * `rebar3 as local shell` 
5. or build and run
   * `rebar3 as local release`
   * `_build/local/rel/cowboy_rest_example/bin/cowboy_rest_example console|foreground`

## Run manual tests(just to see results)
`cd manual_tests && ./all.sh localhost`