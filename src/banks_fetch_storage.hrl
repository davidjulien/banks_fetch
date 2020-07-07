-define(SCHEMA, [
                 {<<"0.0.0">>, <<"0.0.1">>, 
                  [
                   <<"CREATE TABLE banks(id TEXT NOT NULL PRIMARY KEY, name TEXT NOT NULL);">>,
                   <<"CREATE TABLE clients(id SERIAL, bank_id TEXT NOT NULL, client_id TEXT NOT NULL, client_credential BYTEA NOT NULL, PRIMARY KEY (bank_id, client_id));">>,
                   <<"CREATE TYPE e_account_ownership AS ENUM ('single', 'joint');">>,
                   <<"CREATE TYPE e_account_type AS ENUM ('current', 'savings', 'home_loan');">>,
                   <<"CREATE TABLE accounts(id SERIAL, bank_id TEXT NOT NULL, client_id TEXT NOT NULL, account_id TEXT NOT NULL, balance FLOAT NOT NULL, number TEXT NOT NULL, owner TEXT, ",
                     "ownership e_account_ownership NOT NULL, type e_account_type NOT NULL, name TEXT NOT NULL);">>
                  ]}
                ]).
