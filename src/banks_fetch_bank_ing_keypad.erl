%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank_ing_keypad is an external module for banks_fetch_bank_ing dedicated to keypad resolutions.
%% This module is not yet implemented. It allows to ensure full coverage.
%% @end
%%%-------------------------------------------------------------------
-module(banks_fetch_bank_ing_keypad).

-export([
         resolve_keypad/3
        ]).

-spec resolve_keypad(binary(), [non_neg_integer()], string()) -> [[non_neg_integer()]].
resolve_keypad(_KeypadImage, _PinPositions, _ClientPassword) ->
  [].
