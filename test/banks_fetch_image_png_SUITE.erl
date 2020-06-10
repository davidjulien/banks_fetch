-module(banks_fetch_image_png_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0, groups/0,
         should_decode_ing_keypads_1/1,
         should_decode_ing_keypads_2/1,
         should_decode_ing_keypads_3/1
        ]).

all() ->
  [
   {group, ing_keypads}
  ].

groups() -> 
  [
   {ing_keypads, [parallel], [ should_decode_ing_keypads_1, should_decode_ing_keypads_2, should_decode_ing_keypads_3 ]}
  ].

should_decode_ing_keypads_1(Config) ->
  decode_ing_keypads(Config, "keypad_ing_1.png", 2420, 950).

should_decode_ing_keypads_2(Config) ->
  decode_ing_keypads(Config, "keypad_ing_2.png", 2420, 950).

should_decode_ing_keypads_3(Config) ->
  decode_ing_keypads(Config, "keypad_ing_3.png", 2420, 950).

decode_ing_keypads(Config, Filename, ExpectedWidth, ExpectedHeight) ->
  ct:comment("Load ing keypad image"),
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), Filename])),
  {image, Width, Height, Image} = banks_fetch_image_png:decode(KeypadImage),

  ct:comment("Verify image dimension"),
  {ExpectedWidth, ExpectedHeight} = {Width, Height},

  ct:comment("Verify that Image contains the expected number of lines"),
  Height = length(Image),

  ct:comment("Verify that each line has a correct length"),
  lists:foreach(fun(Line) -> Width = length(Line) end, Image),

  ct:comment("Verify first line"),
  ExpectedFirstLine = lists:duplicate(Width, 4),
  ExpectedFirstLine = hd(Image).
