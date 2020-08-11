/*DROP TABLE IF EXISTS users;*/
CREATE TABLE IF NOT EXISTS users (
       id SERIAL PRIMARY KEY,
       email varchar(255) UNIQUE NOT NULL,
       password binary(64) NOT NULL,
       fname varchar(255),
       lname varchar(255),
       created timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
       updated timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP
);