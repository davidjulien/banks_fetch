%% @doc Decode PNG image file format (https://tools.ietf.org/html/rfc2083)
-module(banks_fetch_image_png).

-export([
         decode/1
        ]).

-type image_info() :: {image, Width :: non_neg_integer(), Height :: non_neg_integer(), Pixels :: [ [non_neg_integer()] ]}.

%% PNG always starts with 137 80 78 71 13 10 26 10 
-spec decode(binary()) -> image_info().
decode(<<137,80,78,71,13,10,26,10, Next/binary>>) ->
  start_chunk(Next, none).

start_chunk(<<Length:32, ChunkType:4/binary, Next/binary>>, Info) ->
  <<ChunkData:Length/binary, _CRC:4/binary, Next1/binary>> = Next,
  process_chunk(Next1, ChunkType, ChunkData, Info).

process_chunk(PngData, <<"IHDR">>, ChunkData, _Info) ->
  <<Width:32, Height:32, _BitDepth:8, _ColorType:8, _CompressionMethod:8, _FilterMethod:8, _InterlaceMethod:8>> = ChunkData,
  start_chunk(PngData, {image, Width, Height, []});

process_chunk(_PngData, <<"IEND">>, _, {image, Width, Height, ImageData}) ->
  {image, Width, Height, lists:reverse(ImageData)};

process_chunk(PngData, <<"IDAT">>, ChunkData, {image, Width, Height, Image}) ->
  Uncompress = zlib:uncompress(ChunkData),
  UncompressList = binary_to_list(Uncompress),
  Image2 = decode_image(UncompressList, Width, 0, Image),
  start_chunk(PngData, {image, Width, Height, Image2});

process_chunk(PngData, <<"PLTE">>, ChunkData, Info) ->
  process_plte(ChunkData, 0),
  start_chunk(PngData, Info).

% We need a PNG file with other chunk types to test this part
%process_chunk(PngData, _ChunkType, _ChunkData, Info) ->
%  start_chunk(PngData, Info).




process_plte(<<>>, _Pos) ->
  ok;
process_plte(<<_Red:8, _Green:8, _Blue:8, Rest/binary>>, Pos) ->
  process_plte(Rest,Pos+1).




decode_image([], _, _, Acc) ->
  Acc;
decode_image(List, Width, Pos, Acc) when Acc =/= none ->
  PrevLine = case Acc of
               [] -> lists:seq(0, Width);
               [PrevLine0|_] -> PrevLine0
             end,
  {L1, L2} = lists:split(Width+1, List),
  L1D = apply_filter(hd(L1), tl(L1), PrevLine),
  decode_image(L2, Width, Pos+1, [L1D|Acc]).




% We need a PNG file using filter 0 (none) to test this case
%apply_filter(0, L, _PrevLine) ->
%  L;
apply_filter(1, L, _PrevLine) ->
  apply_filter_sub(L, 0, []);
apply_filter(2, L, PrevLine) ->
  apply_filter_up(L, PrevLine, []);
apply_filter(3, L, PrevLine) ->
  apply_filter_average(L, PrevLine, 0, []);
apply_filter(4, L, PrevLine) ->
  apply_filter_paeth(L, [0|PrevLine], 0, []).


apply_filter_sub([], _Prev, Acc) ->
  lists:reverse(Acc);
apply_filter_sub([X | L], Prev, Acc) ->
  X1 = (X + Prev) rem 256,
  apply_filter_sub(L, X1, [X1|Acc]).


apply_filter_up([], _PrevLine, Acc) ->
  lists:reverse(Acc);
apply_filter_up([X | L], [XPrev | PrevLine], Acc) ->
  X1 = (X + XPrev) rem 256,
  apply_filter_up(L, PrevLine, [X1|Acc]).


apply_filter_average([], _PrevLine, _Left, Acc) ->
  lists:reverse(Acc);
apply_filter_average([X | L], [XPrev | PrevLine], Left, Acc) ->
  X1 = (X + (Left+XPrev) div 2) rem 256,
  apply_filter_average(L, PrevLine, X1, [X1|Acc]).


apply_filter_paeth([], _PrevLine, _Left, Acc) ->
  lists:reverse(Acc);
apply_filter_paeth([X | L], [XUpperLeft, XAbove | PrevLine], Left, Acc) ->
  X1 = (X + paeth_predictor(Left, XAbove, XUpperLeft)) rem 256,
  apply_filter_paeth(L, [XAbove|PrevLine], X1, [X1|Acc]).


paeth_predictor(Left, Above, UpperLeft) ->
  P = Left + Above - UpperLeft,
  PLeft = abs(P-Left),
  PAbove = abs(P-Above),
  PUpperLeft = abs(P-UpperLeft),
  if PLeft =< PAbove andalso PLeft =< PUpperLeft -> Left;
     PAbove =< PUpperLeft -> Above;
     true -> UpperLeft
  end.
