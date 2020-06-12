%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank_ing_keypad is an external module for banks_fetch_bank_ing dedicated to keypad resolutions.
%% This module is not yet implemented. It allows to ensure full coverage.
%% @end
%%%-------------------------------------------------------------------
-module(banks_fetch_bank_ing_keypad).
-include("banks_fetch_bank_ing_keypad.hrl").

-export([
         resolve_keypad/3,

         decode_keypad/1,
         identify_clicks_positions/3
        ]).


-spec resolve_keypad(binary(), [non_neg_integer()], string()) -> [[float()]].
resolve_keypad(KeypadImage, PinPositions, ClientPassword) ->
  KeypadPositions = decode_keypad(KeypadImage),
  identify_clicks_positions(KeypadPositions, PinPositions, ClientPassword).


-spec identify_clicks_positions([{0..9, {non_neg_integer(), non_neg_integer()}, float()}], [non_neg_integer()], string()) -> [[float()]].
identify_clicks_positions(KeypadPositions, PinPositions, ClientPassword) ->
  ClientPasswordList = [ Chr - $0 || Chr <- ClientPassword ],
  MissingNumbers = [ lists:nth(PinPosition, ClientPasswordList) || PinPosition <- PinPositions ],
  [ click_position(lists:keyfind(N, 1, KeypadPositions)) || N <- MissingNumbers ].

click_position({_, {X,Y,Width,Height}, _}) ->
    [X * 1.0 + Width / 2, Y * 1.0 + Height / 2].


% Functions related to keypad decoding
-spec decode_keypad(binary()) -> [{0..9, {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}, float()}].
decode_keypad(Image) ->
  ImageInfo = banks_fetch_image_png:decode(Image),
  Numbers = extract_numbers(ImageInfo),
  Signatures = numbers_to_signatures(Numbers),
  R0 = [ {Coordinates, best_distance(Signature)} || {Coordinates,Signature} <- Signatures ],
  lists:keysort(1, [ {Num, Coordinates, Distance} || {Coordinates, {Distance, Num}} <- R0 ]).

% @doc Extract numbers from keypad image
-spec extract_numbers(banks_fetch_image_png:image_info()) -> [{non_neg_integer(), {non_neg_integer(), non_neg_integer()}, [non_neg_integer()]}].
extract_numbers({image, _Width, Height, Image}) ->
    {Image2, LineNum0} = skip_empty_lines(Image, 0),
    {Numbers1, Image3, LineNum1} = cut_numbers(Image2, [], LineNum0),
    {Image4, LineNum2} = skip_empty_lines(Image3, LineNum1),
    {Numbers2, Image5, LineNum3} = cut_numbers(Image4, Numbers1, LineNum2),
    {[], Height} = skip_empty_lines(Image5, LineNum3),
    Numbers2.

% @doc Build signatures from numbers
numbers_to_signatures(Numbers) ->
    [ {{element(1,Offset), LineNum, element(2,Offset)-element(1,Offset), length(NumberBlock)}, compute_number_signature(NumberBlock)} || {LineNum, Offset, NumberBlock} <- Numbers ].

compute_number_signature(ImageData) ->
    {_, Sig} = lists:foldl(fun(SS, {NLineNum,AccSig}) ->
                SS2 = fill_holes(lists:sublist(SS,140,170), []),
                Form = find_form(SS2),
                {NLineNum+1, [Form | AccSig]}
        end, {100, []}, lists:sublist(ImageData,110,230)),
    SigR = lists:reverse(Sig),
    SigR.


fill_holes([4,O,4|Next], Acc) when O < 4 ->
    fill_holes([4|Next], [4,4|Acc]);
fill_holes([O|Next], Acc) ->
    fill_holes(Next, [O|Acc]);
fill_holes([], Acc) ->
    lists:reverse(Acc).



%% @doc For each precomputed number's signature, evaluate which number in our keypad image is the nearest.
best_distance(S) ->
    lists:foldl(fun({Num, Pattern}, {AccBestScore, AccNum}) ->
                D = distance(S, Pattern, 0),
                if D < AccBestScore -> {D, Num};
                    true -> {AccBestScore, AccNum}
                end
        end, {100000, -1}, ?NUMBERS_SIG).

% @doc Compute distance between two signatures
distance([],[],Acc) ->
    Acc;
distance([A|NA],[A|NB],Acc) ->
   distance(NA, NB, Acc);
distance([LA|NA],[LB|NB],Acc) ->
    distance(NA, NB, distance_aux(LA, LB, Acc)).

distance_aux([], [], Acc) ->
    Acc;
distance_aux([_|_] = L, [], Acc) ->
    Acc + lists:sum([ P || {_,P} <- L ]);
distance_aux([], [_|_] = L, Acc) ->
    Acc + lists:sum([ P || {_,P} <- L ]);
distance_aux([{OffsetA,NA}|NextA],[{OffsetB,NB}|NextB], Acc) ->
    distance_aux(NextA, NextB, Acc+abs(NB-NA)+abs(OffsetA-OffsetB)).


% @doc Skip empty lines in keypad image
skip_empty_lines([], AccLineNum) ->
    {[], AccLineNum};
skip_empty_lines([Line|Next], AccLineNum) ->
    case lists:all(fun(X) -> X =:= 4 end, Line) of
        true -> skip_empty_lines(Next, AccLineNum+1);
        false -> {[Line|Next], AccLineNum}
    end.

is_empty_line(Line) ->
    lists:all(fun(X) -> X =:= 4 end, Line).

cut_numbers([Line|Next], AccNumbers0, AccNumLine0) ->
    Offsets = cut_numbers_offset(Line, 1, {none, []}),
    {AccNumbers2, Next2, AccNumLine1} = cut_numbers_aux(Next, [ {AccNumLine0+1, Offset, []} || Offset <- Offsets], AccNumLine0+1),
    AccNumbers2R = [ {NumLine, Offset, lists:reverse(ImageData)} || {NumLine, Offset, ImageData} <- AccNumbers2 ],
    {AccNumbers0 ++ AccNumbers2R, Next2, AccNumLine1}.


cut_numbers_aux([Line|Next], AccNumbers, AccNumLine0) ->
    case is_empty_line(Line) of
        true -> {AccNumbers, Next, AccNumLine0+1};
        false ->
            AccNumbers2 = [ {NumLine, {Start,End}, [lists:sublist(Line,Start+1,End-Start+1-2)|Acc]} || {NumLine, {Start, End}, Acc} <- AccNumbers ],
            cut_numbers_aux(Next, AccNumbers2, AccNumLine0+1)
    end.

cut_numbers_offset([], _, {_,Acc}) ->
    lists:reverse(Acc);
cut_numbers_offset([N|Next], Pos, Acc) when N =:= 4 orelse N =:= 0 ->
    cut_numbers_offset(Next, Pos+1, Acc);
cut_numbers_offset([N|Next], Pos, {SPos, Acc}) when N =:= 2 orelse N =:= 3 ->
    case SPos of
        none -> cut_numbers_offset(Next, Pos+1, {Pos, Acc});
        _ -> cut_numbers_offset(Next, Pos+1, {none, [{SPos,Pos}|Acc]})
    end.

find_form(Line) ->
    R = find_form_aux(Line, 0, 0, []),
    {_, R2} = lists:foldl(fun({V,N}, {Pos,Acc}) ->
                case V of
                    0 -> {Pos+N, Acc};
                    4 -> {Pos+N, [{Pos,N}|Acc]}
                end
        end, {0,[]}, lists:reverse(R)),
    R2.

find_form_aux([], V, Count, Acc) ->
    lists:reverse([{V,Count}|Acc]);
find_form_aux([NV|Next], V, Count, Acc) when NV > 0 andalso NV < 4 ->
    find_form_aux([0|Next], V, Count, Acc);
find_form_aux([V|Next], V, Count, Acc) ->
    find_form_aux(Next, V, Count+1, Acc);
find_form_aux([NV|Next], V, Count, Acc) ->
    find_form_aux(Next, NV, 1, [{V,Count}|Acc]).
