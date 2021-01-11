INSERT INTO clients(bank_id, client_id, client_credential) VALUES ('ing', 'client1', ''::BYTEA);
INSERT INTO clients(bank_id, client_id, client_credential) VALUES ('ing', 'client2', ''::BYTEA);
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account1', 234.12, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account2', 99.99, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-08T12:00:00Z', 'account1', 426.13, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-09T12:00:00Z', 'account1', 526.14, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-10T12:00:00Z', 'account1', 503.05, 'number1', 'owner1', 'single', 'current', 'CURRENT');
