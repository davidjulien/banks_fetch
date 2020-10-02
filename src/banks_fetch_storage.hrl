-define(SCHEMA, [
                 {<<"0.0.0">>, <<"0.1.0">>,
                  [
                   <<"CREATE TABLE banks(id TEXT NOT NULL PRIMARY KEY, name TEXT NOT NULL);">>,
                   <<"INSERT INTO banks(id, name) VALUES('ing','ING');">>,
                   <<"CREATE TABLE clients(id SERIAL, bank_id TEXT NOT NULL REFERENCES banks(id), client_id TEXT NOT NULL, client_credential BYTEA NOT NULL, PRIMARY KEY (bank_id, client_id));">>,
                   <<"CREATE TYPE e_account_ownership AS ENUM ('single', 'joint');">>,
                   <<"CREATE TYPE e_account_type AS ENUM ('current', 'savings', 'home_loan');">>,
                   <<"CREATE TABLE accounts(id SERIAL, bank_id TEXT NOT NULL, client_id TEXT NOT NULL, fetching_at TIMESTAMP WITHOUT TIME ZONE NOT NULL, "
                     "account_id TEXT NOT NULL, balance FLOAT NOT NULL, number TEXT NOT NULL, owner TEXT, ",
                     "ownership e_account_ownership NOT NULL, type e_account_type NOT NULL, name TEXT NOT NULL, ",
                     "PRIMARY KEY (bank_id, account_id), "
                     "FOREIGN KEY (bank_id, client_id) REFERENCES clients(bank_id, client_id));">>,
                   <<"CREATE TYPE e_transaction_type AS ENUM ('card_debit', 'card_withdrawal', 'check', 'sepa_debit','transfer','interests');">>,
                   <<"CREATE TABLE transactions(id BIGSERIAL, bank_id TEXT NOT NULL, client_id TEXT NOT NULL, account_id TEXT NOT NULL, fetching_at TIMESTAMP WITHOUT TIME ZONE NOT NULL, ",
                     "transaction_id TEXT NOT NULL, accounting_date DATE NOT NULL, effective_date DATE NOT NULL, ",
                     "amount FLOAT NOT NULL, description TEXT NOT NULL, type e_transaction_type NOT NULL, ",
                     "FOREIGN KEY (bank_id, client_id) REFERENCES clients(bank_id, client_id), ",
                     "FOREIGN KEY (bank_id, account_id) REFERENCES accounts(bank_id, account_id));">>
                  ]},
                 {<<"0.1.0">>, <<"0.2.0">>,
                  [
                   <<"CREATE TABLE apps(name TEXT UNIQUE NOT NULL, version TEXT NOT NULL)">>,
                   <<"INSERT INTO apps(name, version) VALUES('banks_fetch', '0.2.0')">>
                  ]},
                 {<<"0.2.0">>, <<"0.2.1">>,
                  [
                   <<"UPDATE apps SET version = '0.2.1' where name = 'banks_fetch';">>,
                   <<"ALTER TABLE transactions DROP CONSTRAINT IF EXISTS transactions_bank_id_account_id_fkey, DROP CONSTRAINT IF EXISTS transactions_bank_id_fkey, DROP CONSTRAINT IF EXISTS transactions_bank_id_fkey1;">>,
                   <<"ALTER TABLE accounts DROP CONSTRAINT accounts_pkey;">>,
                   <<"ALTER TABLE transactions ADD COLUMN fetching_position INTEGER NOT NULL;">>,
                   <<"DELETE FROM transactions a USING transactions b WHERE a.fetching_at > b.fetching_at AND a.transaction_id = b.transaction_id and a.bank_id = b.bank_id and a.client_id = b.client_id;">>,
                   <<"CREATE UNIQUE INDEX transactions_bank_client_transaction_ids ON transactions(bank_id,client_id,transaction_id);">>
                  ]},
                 {<<"0.2.1">>, <<"0.2.2">>,
                  [
                   % Because ALTER TYPE e_transaction_type ADD VALUE 'other'; does not work in transaction block, we have to convert column type in transactions, change type and convert again to new type definition
                   <<"ALTER TABLE transactions ALTER COLUMN \"type\" type VARCHAR(255);">>,
                   <<"DROP TYPE IF EXISTS e_transaction_type">>,
                   <<"CREATE TYPE e_transaction_type AS ENUM ('card_debit', 'card_withdrawal', 'check', 'sepa_debit','transfer','interests','other');">>,
                   <<"ALTER TABLE transactions ALTER COLUMN type TYPE e_transaction_type USING (type::e_transaction_type);">>
                  ]
                 }
                ]).
