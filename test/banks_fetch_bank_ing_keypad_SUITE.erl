-module(banks_fetch_bank_ing_keypad_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         groups/0,
         should_decode_keypad_1/1,
         should_decode_keypad_2/1,
         should_decode_keypad_3/1,
         should_identify_where_to_click_1/1,
         should_identify_where_to_click_2/1,
         should_resolve_keypad_1/1
        ]).

all() ->
  [
   {group, decoding},
   {group, positions},
   {group, resolve}
  ].

groups() ->
  [{decoding, [parallel], [should_decode_keypad_1, should_decode_keypad_2, should_decode_keypad_3]},
   {positions, [parallel], [should_identify_where_to_click_1, should_identify_where_to_click_2]},
   {resolve, [parallel], [should_resolve_keypad_1]}
  ].

-define(KEYPAD_POSITIONS_1,
        [
         {0,{500,16,451,442},0},
         {1,{16,16,451,442},0},
         {2,{984,16,451,442},0},
         {3,{1952,491,451,442},0},
         {4,{1468,16,451,442},0},
         {5,{500,491,451,442},0},
         {6,{16,491,451,442},0},
         {7,{1468,491,451,442},0},
         {8,{984,491,451,442},0},
         {9,{1952,16,451,442},0}
        ]).

-define(KEYPAD_POSITIONS_2,
        [
         {0,{1468,491,451,442},179},
         {1,{500,491,451,442},23},
         {2,{16,491,451,442},138},
         {3,{1952,16,451,442},149},
         {4,{984,491,451,442},121},
         {5,{984,16,451,442},378},
         {6,{1468,16,451,442},168},
         {7,{1952,491,451,442},3},
         {8,{500,16,451,442},186},
         {9,{16,16,451,442},0}
        ]).

-define(KEYPAD_POSITIONS_3,
        [
         {0,{1468,16,451,442},0},
         {1,{984,16,451,442},0},
         {2,{16,16,451,442},0},
         {3,{500,491,451,442},5},
         {4,{1952,491,451,442},121},
         {5,{984,491,451,442},2},
         {6,{16,491,451,442},0},
         {7,{1468,491,451,442},0},
         {8,{1952,16,451,442},185},
         {9,{500,16,451,442},7}
        ]).

should_decode_keypad_1(Config) ->
  ct:comment("Load keypad image"),
  % This image has been used to build our numbers' signatures
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), "keypad_ing_1.png"])),
  ct:comment("Verify keypad decoding"),
  KeypadPositions = banks_fetch_bank_ing_keypad:decode_keypad(KeypadImage),
  ct:comment("Verify that extracted positions are exact"),
  ?KEYPAD_POSITIONS_1 = KeypadPositions.

should_decode_keypad_2(Config) ->
  ct:comment("Load keypad image"),
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), "keypad_ing_2.png"])),
  ct:comment("Verify keypad decoding"),
  KeypadPositions = banks_fetch_bank_ing_keypad:decode_keypad(KeypadImage),
  ct:comment("Verify that extracted positions are exact"),
  ?KEYPAD_POSITIONS_2 = KeypadPositions.

should_decode_keypad_3(Config) ->
  ct:comment("Load keypad image"),
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), "keypad_ing_3.png"])),
  ct:comment("Verify keypad decoding"),
  KeypadPositions = banks_fetch_bank_ing_keypad:decode_keypad(KeypadImage),
  ct:comment("Verify that extracted positions are exact"),
  ?KEYPAD_POSITIONS_3 = KeypadPositions.



should_identify_where_to_click_1(_Config) ->
  ct:comment("Verify click positions"),
  ClickPositions = banks_fetch_bank_ing_keypad:identify_clicks_positions(?KEYPAD_POSITIONS_1, [1,2,3], "123456"),
  [[241.5,237.0],[1209.5,237.0],[2177.5,712.0]] = ClickPositions.

should_identify_where_to_click_2(_Config) ->
  ct:comment("Verify click positions"),
  ClickPositions = banks_fetch_bank_ing_keypad:identify_clicks_positions(?KEYPAD_POSITIONS_1, [4,5,6], "123456"),
  [[1693.5,237.0],[725.5,712.0],[241.5,712.0]] = ClickPositions.


should_resolve_keypad_1(Config) ->
  ct:comment("Load keypad image"),
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), "keypad_ing_1.png"])),
  ct:comment("Verify keypad resolution"),
  ClickPositions = banks_fetch_bank_ing_keypad:resolve_keypad(KeypadImage, [1,2,3], "123456"),
  [[241.5,237.0],[1209.5,237.0],[2177.5,712.0]] = ClickPositions.
