banks_fetch
=====

An OTP application to fetch banks accounts and transactions.

banks_fetch is not working yet.

Current status of banks modules :

| Banks         | connect       | accounts  | transactions |
| ------------- |:-------------:|:---------:|:------------:|
| ING           | OK            | OK        | -            |


Build
-----

    $ rebar3 compile

Tests
-----

Full verifications (xref, dialyzer, ct and code coverage) :

    $ rebar3 check

To test a `BANK` with your own credential, you just need to add a file containing it. See `test_with_real_credential` function in `test/*BANK_SUITE.erl` to identify required file and expected content.
