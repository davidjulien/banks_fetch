INSERT INTO clients(bank_id, client_id, client_credential) VALUES('ing', 'client1', '\x8368016d0000000b63726564656e7469616c31'::BYTEA);
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account1', 234.12, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account2', 4321.78, 'number2', 'owner2', 'single', 'savings', 'LDD');
INSERT INTO stores(id, name) VALUES (1, 'BOUTIQUE 1'), (2, 'PETITEBOUTIQUE'), (3, 'URSSAF'), (4, 'SYNDIC');
INSERT INTO mappings(id, pattern, fix_date, period, budget_id, categories_ids, store_id) VALUES (1, 'PETITEBOUTIQUE', 'none', 'month', 1, ARRAY[1,52], 2);
INSERT INTO mappings(id, pattern, fix_date, period, budget_id, categories_ids, store_id) VALUES (2, 'URSSAF', 'previous', 'month', 1, ARRAY[1,42], 3);
INSERT INTO mappings(id, pattern, fix_date, period, budget_id, categories_ids, store_id) VALUES (3, 'CHARGES', 'next_if_end', 'quarter', 1, ARRAY[1,42], 4);
