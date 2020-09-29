INSERT INTO clients(bank_id, client_id, client_credential) VALUES ('ing', 'client1', ''::BYTEA);
INSERT INTO clients(bank_id, client_id, client_credential) VALUES ('ing', 'client2', ''::BYTEA);
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account1', 234.12, 'number1', 'owner1', 'single', 'current', 'CURRENT');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client1', '2020-07-07T12:00:00Z', 'account2', 4321.78, 'number2', 'owner2', 'single', 'savings', 'LDD');
INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES ('ing', 'client2', '2020-07-07T12:00:00Z', 'account3', 5675.44, 'number3', 'owner3', 'joint', 'current', 'CURRENT');
INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES ('ing', 'client1', 'account1', '2020-07-08T14:00:00Z', 0, 'transaction1', '2020-07-08', '2020-07-08', -44.44, 'PAIEMENT PAR CARTE', 'card_debit');
INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES ('ing', 'client1', 'account1', '2020-07-08T14:00:00Z', 1, 'transaction2', '2020-07-06', '2020-07-06', -88.88, 'RETRAIT DAB', 'card_withdrawal');
INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES ('ing', 'client1', 'account2', '2020-07-08T14:00:10Z', 0, 'transaction3', '2020-07-08', '2020-07-08', -77.77, 'VIREMENT SEPA', 'sepa_debit');
INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES ('ing', 'client1', 'account2', '2020-07-08T14:00:10Z', 1, 'transaction4', '2020-07-08', '2020-07-08', -66.66, 'VIREMENT SEPA', 'sepa_debit');
INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES ('ing', 'client2', 'account3', '2020-07-08T14:00:20Z', 0, 'transaction5', '2020-07-07', '2020-07-07', -55.55, 'VIREMENT SEPA', 'sepa_debit');
