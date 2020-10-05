INSERT INTO clients(bank_id, client_id, client_credential) VALUES('ing', 'client1', '\x8368016d0000000b63726564656e7469616c31'::BYTEA);
INSERT INTO clients(bank_id, client_id, client_credential) VALUES('ing', 'client2', '\x8368016d0000000b63726564656e7469616c31'::BYTEA);
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account1', 234.12, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account2', 4321.78, 'number2', 'owner2', 'single', 'savings', 'LDD');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client2', '2020-07-07T14:00:00Z', 'account3', 431.80, 'number3', 'owner3', 'single', 'savings', 'LDD');
