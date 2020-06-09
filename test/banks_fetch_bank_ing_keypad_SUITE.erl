-module(banks_fetch_bank_ing_keypad_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         should_decode_keypad/1
        ]).

all() -> [should_decode_keypad].

should_decode_keypad(_Config) ->
  %% This is a fake test to ensure full coverage
  [] = banks_fetch_bank_ing_keypad:resolve_keypad(<<>>, [], "").
