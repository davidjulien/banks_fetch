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
                 },
                 {<<"0.2.2">>, <<"0.2.3">>,
                  [
                   % Because ALTER TYPE e_transaction_type ADD VALUE 'bank_fees'; does not work in transaction block, we have to convert column type in transactions, change type and convert again to new type definition
                   <<"ALTER TABLE transactions ALTER COLUMN \"type\" type VARCHAR(255);">>,
                   <<"DROP TYPE IF EXISTS e_transaction_type">>,
                   <<"CREATE TYPE e_transaction_type AS ENUM ('other','card_debit', 'card_withdrawal', 'check', 'sepa_debit','transfer','interests','bank_fees');">>,
                   <<"ALTER TABLE transactions ALTER COLUMN type TYPE e_transaction_type USING (type::e_transaction_type);">>
                  ]
                 },
                 % Each transaction may be associated to a budget (everyday purchases, fun, exceptional...), a category (grocery, restaurant...) and a store.
                 % You may have also a real date (when you pay something at the beginning of the month for the previous month) and period (like one payment for a quarter)
                 % Mapping table contains pattern allowing to enrich transactions automatically. Enrichment is done at insertion or when description is updated
                 {<<"0.2.3">>, <<"0.2.4">>,
                  [
                   <<"CREATE TABLE budgets(id SERIAL PRIMARY KEY, name TEXT NOT NULL);">>,
                   <<"CREATE TABLE categories(id SERIAL PRIMARY KEY, up_category_id INTEGER, name TEXT NOT NULL);">>,
                   <<"CREATE OR REPLACE FUNCTION compute_real_date(effective_date DATE, description TEXT) RETURNS DATE AS $$ SELECT CASE WHEN description ~* '^(VIREMENT|VIR|PRLV|AVOIR CARTE) ' THEN effective_date WHEN description ~* '^(CARTE|PAIEMENT PAR CARTE) '  THEN to_date(substring(description from '^(?:CARTE|PAIEMENT PAR CARTE) ([^ ]*) '),'DD/MM/YYYY') WHEN description ~* '^RETRAIT DAB( | .* )../../....' THEN to_date(substring(description FROM '^RETRAIT DAB(?: | .* )(../../....*) '),'DD/MM/YYYY') WHEN description ~* 'RETRAIT DAB [^0-9]' THEN effective_date else null END $$ LANGUAGE sql">>,
                   <<"CREATE TYPE e_fix_date AS ENUM ('previous2','previous','previous_if_begin','none','next','next_if_end')">>,
                   <<"CREATE TYPE e_period AS ENUM('bimester','quarter','semester','annual')">>,
                   <<"CREATE TABLE mapping(id SERIAL PRIMARY KEY, pattern TEXT NOT NULL, fix_date e_fix_date, period e_period, budget_id INTEGER, categories_id INTEGER[], store_id INTEGER);">>,
                   <<"CREATE UNIQUE INDEX mapping_pattern_idx ON mapping(pattern);">>,
                   <<"CREATE TABLE stores(id SERIAL PRIMARY KEY, name TEXT NOT NULL)">>,
                   <<"CREATE UNIQUE INDEX stores_name_idx ON stores(name);">>,
                   <<"ALTER TABLE transactions ADD COLUMN ext_mapping_id INTEGER, ADD COLUMN ext_date DATE, ADD COLUMN ext_period e_period, ADD COLUMN ext_budget_id INTEGER, ADD COLUMN ext_categories_id INTEGER[], ADD COLUMN ext_store_id INTEGER;">>,
                   <<"CREATE FUNCTION analyze_transaction() RETURNS trigger AS $analyze_transaction$\n",
                     "DECLARE selected_mapping mapping%rowtype;\n",
                     "BEGIN\n",
                     " SELECT * INTO selected_mapping FROM mapping WHERE NEW.description ~* mapping.pattern order by length(mapping.pattern) desc, mapping.id limit 1;\n",
                     " IF NOT FOUND THEN\n",
                     "   NEW.ext_date = compute_real_date(NEW.effective_date, NEW.description);\n",
                     "   RETURN NEW;\n",
                     " END IF;\n",
                     " NEW.ext_date = CASE WHEN selected_mapping.fix_date is null THEN compute_real_date(NEW.effective_date, NEW.description)\n",
                     "                     WHEN selected_mapping.fix_date = 'previous2' THEN date_trunc('month', NEW.effective_date) - INTERVAL '1 month' - INTERVAL '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous' THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) < 15 THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) >= 15 THEN NEW.effective_date",
                     "                     WHEN selected_mapping.fix_date = 'next' THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) >= 15 THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) < 15 THEN NEW.effective_date",
                     "                END;\n",
                     " NEW.ext_mapping_id = selected_mapping.id;\n",
                     " NEW.ext_period = selected_mapping.period;\n",
                     " NEW.ext_budget_id = selected_mapping.budget_id;\n",
                     " NEW.ext_categories_id = selected_mapping.categories_id;\n",
                     " NEW.ext_store_id = selected_mapping.store_id;\n",
                     " RETURN NEW;\n",
                     "END; $analyze_transaction$ LANGUAGE plpgsql;">>,
                   <<"CREATE TRIGGER analyze_transaction BEFORE INSERT ON transactions FOR EACH ROW EXECUTE PROCEDURE analyze_transaction();">>,
                   <<"CREATE TRIGGER update_transaction BEFORE UPDATE OF description ON transactions FOR EACH ROW EXECUTE PROCEDURE analyze_transaction();">>
                  ]
                 },
                 {<<"0.2.4">>, <<"0.2.5">>,
                  [
                   <<"ALTER TABLE mapping RENAME TO mappings;">>,
                   <<"alter sequence mapping_id_seq restart with 1000000;">>,
                   <<"CREATE OR REPLACE FUNCTION analyze_transaction() RETURNS trigger AS $analyze_transaction$\n",
                     "DECLARE selected_mapping mappings%rowtype;\n",
                     "BEGIN\n",
                     "IF NEW.ext_mapping_id >= 1000000 THEN\n",
                     "  RETURN NEW;\n"
                     "ELSE\n",
                     " SELECT * INTO selected_mapping FROM mappings WHERE NEW.description ~* mappings.pattern order by length(mappings.pattern) desc, mappings.id limit 1;\n",
                     " IF NOT FOUND THEN\n",
                     "   NEW.ext_date = compute_real_date(NEW.effective_date, NEW.description);\n",
                     "   NEW.ext_period = 'month';\n",
                     "   RETURN NEW;\n",
                     " END IF;\n",
                     " NEW.ext_date = CASE WHEN selected_mapping.fix_date = 'none' THEN compute_real_date(NEW.effective_date, NEW.description)\n",
                     "                     WHEN selected_mapping.fix_date = 'previous2' THEN date_trunc('month', NEW.effective_date) - INTERVAL '1 month' - INTERVAL '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous' THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) < 15 THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) >= 15 THEN NEW.effective_date",
                     "                     WHEN selected_mapping.fix_date = 'next' THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) >= 15 THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) < 15 THEN NEW.effective_date",
                     "                END;\n",
                     " NEW.ext_mapping_id = selected_mapping.id;\n",
                     " NEW.ext_period = selected_mapping.period;\n",
                     " NEW.ext_budget_id = selected_mapping.budget_id;\n",
                     " NEW.ext_categories_id = selected_mapping.categories_id;\n",
                     " NEW.ext_store_id = selected_mapping.store_id;\n",
                     " RETURN NEW;\n",
                     "END IF;\n",
                     "END; $analyze_transaction$ LANGUAGE plpgsql;">>,

                   <<"ALTER TABLE mappings ALTER COLUMN period type VARCHAR(255);">>,
                   <<"ALTER TABLE transactions ALTER COLUMN ext_period type VARCHAR(255);">>,
                   <<"DROP TYPE IF EXISTS e_period">>,
                   <<"CREATE TYPE e_period AS ENUM('month','bimester','quarter','semester','annual')">>,
                   <<"ALTER TABLE transactions ALTER COLUMN ext_period TYPE e_period USING (ext_period::e_period);">>,
                   <<"ALTER TABLE mappings ALTER COLUMN period TYPE e_period USING (period::e_period);">>,
                   <<"ALTER TABLE mappings ALTER COLUMN period SET NOT NULL, ALTER COLUMN fix_date SET NOT NULL;">>
                  ]
                 },
                 {<<"0.2.5">>, <<"0.2.6">>,
                  [
                   <<"CREATE OR REPLACE FUNCTION analyze_transaction() RETURNS trigger AS $analyze_transaction$\n",
                     "DECLARE selected_mapping mappings%rowtype;\n",
                     "BEGIN\n",
                     "IF NEW.ext_mapping_id >= 1000000 THEN\n",
                     "  RETURN NEW;\n"
                     "ELSE\n",
                     " SELECT * INTO selected_mapping FROM mappings WHERE NEW.description ~* mappings.pattern order by length(mappings.pattern) desc, mappings.id limit 1;\n",
                     " IF NOT FOUND THEN\n",
                     "   IF NEW.ext_date IS NULL THEN\n",
                     "     NEW.ext_date = compute_real_date(NEW.effective_date, NEW.description);\n",
                     "   END IF;"
                     "   RETURN NEW;\n",
                     " END IF;\n",
                     " NEW.ext_date = CASE WHEN selected_mapping.fix_date = 'none' THEN compute_real_date(NEW.effective_date, NEW.description)\n",
                     "                     WHEN selected_mapping.fix_date = 'previous2' THEN date_trunc('month', NEW.effective_date) - INTERVAL '1 month' - INTERVAL '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous' THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) < 15 THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) >= 15 THEN NEW.effective_date",
                     "                     WHEN selected_mapping.fix_date = 'next' THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) >= 15 THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) < 15 THEN NEW.effective_date",
                     "                END;\n",
                     " NEW.ext_mapping_id = selected_mapping.id;\n",
                     " NEW.ext_period = selected_mapping.period;\n",
                     " NEW.ext_budget_id = selected_mapping.budget_id;\n",
                     " NEW.ext_categories_id = selected_mapping.categories_id;\n",
                     " NEW.ext_store_id = selected_mapping.store_id;\n",
                     " RETURN NEW;\n",
                     "END IF;\n",
                     "END; $analyze_transaction$ LANGUAGE plpgsql;">>,
                   <<"alter sequence stores_id_seq restart with 1000000;">>
                  ]
                 },
                 {<<"0.2.6">>, <<"0.2.7">>,
                  [ % Transaction may be splitted in sub-transactions
                   <<"ALTER TABLE transactions ADD COLUMN ext_splitted BOOLEAN NOT NULL DEFAULT FALSE;">>,
                   <<"ALTER TABLE transactions ADD COLUMN ext_split_of_id TEXT;">>
                  ]
                 },
                 {<<"0.2.7">>, <<"0.2.8">>,
                  [
                   <<"CREATE OR REPLACE FUNCTION analyze_transaction() RETURNS trigger AS $analyze_transaction$\n",
                     "DECLARE selected_mapping mappings%rowtype;\n",
                     "BEGIN\n",
                     "IF NEW.ext_mapping_id >= 1000000 OR NEW.ext_mapping_id < 0 THEN\n", % Private mapping or fields updated manually
                     "  RETURN NEW;\n"
                     "ELSE\n",
                     " SELECT * INTO selected_mapping FROM mappings WHERE NEW.description ~* mappings.pattern order by length(mappings.pattern) desc, mappings.id limit 1;\n",
                     " IF NOT FOUND THEN\n",
                     "   IF NEW.ext_date IS NULL THEN\n",
                     "     NEW.ext_date = compute_real_date(NEW.effective_date, NEW.description);\n",
                     "   END IF;"
                     "   RETURN NEW;\n",
                     " END IF;\n",
                     " NEW.ext_date = CASE WHEN selected_mapping.fix_date = 'none' THEN compute_real_date(NEW.effective_date, NEW.description)\n",
                     "                     WHEN selected_mapping.fix_date = 'previous2' THEN date_trunc('month', NEW.effective_date) - INTERVAL '1 month' - INTERVAL '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous' THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) < 15 THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) >= 15 THEN NEW.effective_date",
                     "                     WHEN selected_mapping.fix_date = 'next' THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) >= 15 THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) < 15 THEN NEW.effective_date",
                     "                END;\n",
                     " NEW.ext_mapping_id = selected_mapping.id;\n",
                     " NEW.ext_period = selected_mapping.period;\n",
                     " NEW.ext_budget_id = selected_mapping.budget_id;\n",
                     " NEW.ext_categories_id = selected_mapping.categories_id;\n",
                     " NEW.ext_store_id = selected_mapping.store_id;\n",
                     " RETURN NEW;\n",
                     "END IF;\n",
                     "END; $analyze_transaction$ LANGUAGE plpgsql;">>
                  ]
                 },
                 {<<"0.2.8">>, <<"0.2.9">>,
                  [
                   <<"ALTER TABLE transactions RENAME COLUMN ext_categories_id TO ext_categories_ids">>,
                   <<"ALTER TABLE mappings RENAME COLUMN categories_id TO categories_ids">>,
                   <<"CREATE OR REPLACE FUNCTION analyze_transaction() RETURNS trigger AS $analyze_transaction$\n",
                     "DECLARE selected_mapping mappings%rowtype;\n",
                     "BEGIN\n",
                     "IF NEW.ext_mapping_id >= 1000000 OR NEW.ext_mapping_id < 0 THEN\n", % Private mapping or fields updated manually
                     "  RETURN NEW;\n"
                     "ELSE\n",
                     " SELECT * INTO selected_mapping FROM mappings WHERE NEW.description ~* mappings.pattern order by length(mappings.pattern) desc, mappings.id limit 1;\n",
                     " IF NOT FOUND THEN\n",
                     "   IF NEW.ext_date IS NULL THEN\n",
                     "     NEW.ext_date = compute_real_date(NEW.effective_date, NEW.description);\n",
                     "   END IF;"
                     "   RETURN NEW;\n",
                     " END IF;\n",
                     " NEW.ext_date = CASE WHEN selected_mapping.fix_date = 'none' THEN compute_real_date(NEW.effective_date, NEW.description)\n",
                     "                     WHEN selected_mapping.fix_date = 'previous2' THEN date_trunc('month', NEW.effective_date) - INTERVAL '1 month' - INTERVAL '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous' THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) < 15 THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) >= 15 THEN NEW.effective_date",
                     "                     WHEN selected_mapping.fix_date = 'next' THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) >= 15 THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) < 15 THEN NEW.effective_date",
                     "                END;\n",
                     " NEW.ext_mapping_id = selected_mapping.id;\n",
                     " NEW.ext_period = selected_mapping.period;\n",
                     " NEW.ext_budget_id = selected_mapping.budget_id;\n",
                     " NEW.ext_categories_ids = selected_mapping.categories_ids;\n",
                     " NEW.ext_store_id = selected_mapping.store_id;\n",
                     " RETURN NEW;\n",
                     "END IF;\n",
                     "END; $analyze_transaction$ LANGUAGE plpgsql;">>
                  ]
                 },
                 {<<"0.2.9">>, <<"0.2.10">>,
                  [
                   <<"CREATE OR REPLACE FUNCTION analyze_transaction() RETURNS trigger AS $analyze_transaction$\n",
                     "DECLARE selected_mapping mappings%rowtype;\n",
                     "BEGIN\n",
                     "IF NEW.ext_mapping_id >= 1000000 OR NEW.ext_mapping_id < 0 THEN\n", % Private mapping or fields updated manually
                     "  RETURN NEW;\n"
                     "ELSE\n",
                     " SELECT * INTO selected_mapping FROM mappings WHERE NEW.description ~* mappings.pattern order by length(substring(NEW.description FROM mappings.pattern)) desc, mappings.id limit 1;\n",
                     " IF NOT FOUND THEN\n",
                     "   IF NEW.ext_date IS NULL THEN\n",
                     "     NEW.ext_date = compute_real_date(NEW.effective_date, NEW.description);\n",
                     "   END IF;"
                     "   RETURN NEW;\n",
                     " END IF;\n",
                     " NEW.ext_date = CASE WHEN selected_mapping.fix_date = 'none' THEN compute_real_date(NEW.effective_date, NEW.description)\n",
                     "                     WHEN selected_mapping.fix_date = 'previous2' THEN date_trunc('month', NEW.effective_date) - INTERVAL '1 month' - INTERVAL '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous' THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) < 15 THEN date_trunc('month', NEW.effective_date) - interval '1 day'",
                     "                     WHEN selected_mapping.fix_date = 'previous_if_begin' AND date_part('day', NEW.effective_date) >= 15 THEN NEW.effective_date",
                     "                     WHEN selected_mapping.fix_date = 'next' THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) >= 15 THEN date_trunc('month', NEW.effective_date) + INTERVAL '1 month'",
                     "                     WHEN selected_mapping.fix_date = 'next_if_end' AND date_part('day', NEW.effective_date) < 15 THEN NEW.effective_date",
                     "                END;\n",
                     " NEW.ext_mapping_id = selected_mapping.id;\n",
                     " NEW.ext_period = selected_mapping.period;\n",
                     " NEW.ext_budget_id = selected_mapping.budget_id;\n",
                     " NEW.ext_categories_ids = selected_mapping.categories_ids;\n",
                     " NEW.ext_store_id = selected_mapping.store_id;\n",
                     " RETURN NEW;\n",
                     "END IF;\n",
                     "END; $analyze_transaction$ LANGUAGE plpgsql;">>
                  ]
                 },
                 {<<"0.2.10">>, <<"0.2.11">>,
                  [
                   <<"INSERT INTO banks(id, name) VALUES('purse','Purse');">>,

                   <<"ALTER TABLE accounts ALTER COLUMN type TYPE VARCHAR(255);">>,
                   <<"DROP TYPE IF EXISTS e_account_type">>,
                   <<"CREATE TYPE e_account_type AS ENUM ('current', 'savings', 'home_loan', 'purse');">>,
                   <<"ALTER TABLE accounts ALTER COLUMN type TYPE e_account_type USING (type::e_account_type);">>
                  ]
                 },
                 {<<"0.2.11">>, <<"0.2.12">>,
                  [
                   % Because ALTER TYPE e_transaction_type ADD VALUE; does not work in transaction block, we have to convert column type in transactions, change type and convert again to new type definition
                   <<"ALTER TABLE transactions ALTER COLUMN \"type\" type VARCHAR(255);">>,
                   <<"DROP TYPE IF EXISTS e_transaction_type">>,
                   <<"CREATE TYPE e_transaction_type AS ENUM ('other','card_debit', 'card_withdrawal', 'check', 'sepa_debit','transfer','interests','bank_fees','stock');">>,
                   <<"ALTER TABLE transactions ALTER COLUMN type TYPE e_transaction_type USING (type::e_transaction_type);">>,
                   <<"INSERT INTO banks(id, name) VALUES('boursedirect','Bourse Direct');">>,
                   <<"ALTER TABLE accounts ALTER COLUMN type TYPE VARCHAR(255);">>,
                   <<"DROP TYPE IF EXISTS e_account_type">>,
                   <<"CREATE TYPE e_account_type AS ENUM ('current', 'savings', 'home_loan', 'purse', 'markets');">>,
                   <<"ALTER TABLE accounts ALTER COLUMN type TYPE e_account_type USING (type::e_account_type);">>
                  ]
                 }
                ]).
