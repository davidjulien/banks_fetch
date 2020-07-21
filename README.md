banks_fetch [![Build Status](https://travis-ci.com/davidjulien/banks_fetch.svg?branch=master)](https://travis-ci.com/davidjulien/banks_fetch)
===========

An OTP application to fetch banks accounts and transactions.

banks_fetch has no release yet but it can be started with rebar shell (see below).

Current status of banks modules :

| Banks         | connect       | accounts  | transactions |
| ------------- |:-------------:|:---------:|:------------:|
| ING           | OK            | OK        | OK           |


Build
-----

    $ rebar3 compile

Run
-----

It is possible to run banks_fetch with rebar shell.

1. Setup an empty banks_fetch database (if this database does not exist)

```console
psql
```

```sql
CREATE DATABASE banks_fetch;
CREATE ROLE banks_fetch_user WITH LOGIN;
ALTER USER banks_fetch_user WITH SUPERUSER;
GRANT ALL PRIVILEGES ON DATABASE banks_fetch TO banks_fetch_user;
ALTER DATABASE banks_fetch OWNER TO banks_fetch_user;
```

2. Start rebar shell

```console
rebar3 shell
```

3. Add an ING bank account

```erlang
banks_fetch_client_manager:add_client({bank_id, <<"ing">>}, {client_id, <<"YOUR_CLIENT_ID">>}, {client_credential, {"YOUR_PASSWORD","YOUR_BIRTHDATE_DDMMYYYY"}}).
```

It will connect to your bank and fetch accounts data every 4 hours. Credential (and accounts data) will be stored in postgres database and client will be started again automatically each time rebar shell is started.

Tests
-----

Full verifications (xref, dialyzer, ct and code coverage) :

```console
rebar3 check
```

To test a `BANK` with your own credential, you just need to add a file containing it. See `test_with_real_credential` function in `test/*BANK_SUITE.erl` to identify required file and expected content.

An integration test is available in `test/banks_fetch__integration_SUITE.erl` if you add a file containing bank credential in `test/banks_fetch__integration_SUITE_data/real_credential.hrl`
