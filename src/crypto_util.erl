-module(crypto_util).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

decrypt_single_xors(Data) ->
    lists:foldl(fun(X, {C,Sc,St}) ->
                        Xord = single_xors(Data, X),
                        case analyze_english:score(Xord, 0.7) of
                            Score when Score > Sc ->
                                {X, Score, Xord};
                            _ ->
                                {C, Sc, St}
                        end
                end, {0,0,""}, lists:seq(0,255)).

decrypt_repeating_xors(Data) ->
    [{_, Keysize}|_] = guess_keysize(Data, 2, 40),
    Blocks = keysize_blocks(Data, Keysize, []),
    Transposed = transpose_blocks(Blocks),
    Key = guess_key(Transposed, []),
    repeating_xors(Data, Key).

keysize_blocks([], _, Accum) ->
    lists:reverse(Accum);
keysize_blocks(Data, Keysize, Accum) ->
    case length(Data) of
        L when L < Keysize ->
            Chunk = lists:sublist(Data, Keysize),
            keysize_blocks([], Keysize, [Chunk|Accum]);
        _ ->
            Chunk = lists:sublist(Data, Keysize),
            Rest = lists:nthtail(Keysize, Data),
            keysize_blocks(Rest, Keysize, [Chunk|Accum])
    end.

guess_keysize(Data, Start, End) ->
     guess_keysize(Data, Start, End, []).

guess_keysize(_, Start, End, Guesses) when Start > End ->
    lists:keysort(1, Guesses);
guess_keysize(Data, Start, End, Guesses) ->
    NormGuess = normalized_hamming_distance(Data, Start),
    guess_keysize(Data, Start+1, End, [{NormGuess, Start} | Guesses]).

normalized_hamming_distance(Data, Keysize) ->
    TotalChunks = length(Data) / Keysize,
    TotalDist = total_hamming_distance(Data, Keysize, 0),
    TotalDist / (TotalChunks * Keysize).

total_hamming_distance([], _, Accum) ->
    Accum;
total_hamming_distance(Data, Keysize, Accum) ->
    case length(Data) of
        L when L < (2 * Keysize) -> Accum;
        _ ->
            Dist = keysize_hamming_distance(Data, Keysize),
            total_hamming_distance(lists:nthtail(Keysize, Data), Keysize, Dist + Accum)
    end.

keysize_hamming_distance(Data, Keysize) ->
   C1 = lists:sublist(Data, Keysize),
    C2 = lists:sublist(Data, Keysize+1, Keysize),
    hamming_distance(list_to_binary(C1), list_to_binary(C2)).

hamming_distance(Data1, Data2) ->
    hamming_distance(Data1, Data2, 0).

hamming_distance(<<"">>, <<"">>, Accum) ->
    Accum;
hamming_distance(<<D1:1/bitstring, R1/bitstring>>, <<D1:1/bitstring, R2/bitstring>>, Accum) ->
    hamming_distance(R1, R2, Accum);
hamming_distance(<<_:1/bitstring, R1/bitstring>>, <<_:1/bitstring, R2/bitstring>>, Accum) ->
    hamming_distance(R1, R2, Accum + 1).

guess_key([], Acc) ->
    lists:reverse(Acc);
guess_key([T|R], Acc) ->
    {Char, _, _} = decrypt_single_xors(T),
    guess_key(R, [Char|Acc]).

transpose_blocks(Blocks) ->
    transpose_blocks(Blocks, []).

transpose_blocks(Blocks, Accum) ->
    case first_byte_block(Blocks, [], []) of
        {[[]|_], LastBlock} ->
            lists:reverse([LastBlock|Accum]);
        {Remaining, Block} ->
            transpose_blocks(Remaining, [Block|Accum])
    end.

first_byte_block([[]], Remaining, Accum) ->
    {lists:reverse(Remaining), lists:reverse(Accum)};
first_byte_block([], Remaining, Accum) ->
    {lists:reverse(Remaining), lists:reverse(Accum)};
first_byte_block([[H|T]|Rest], Remaining, Accum) ->
    first_byte_block(Rest, [T|Remaining], [H|Accum]).

file_as_lines(Filename) ->
    F = fun(Entry, Accum) ->
                [Entry|Accum]
        end,
    for_each_line_in_file(Filename, F, [read], []).

file_to_string(Filename) ->
    %% F = fun(Entry, Acc) -> [Entry|Acc] end,
    %% R = for_each_line_in_file(Filename, F, [read], []),
    %% string:strip(lists:flatten(lists:reverse(R)), both, $\n).
    {ok, Bin} = file:read_file(Filename),
    string:strip(binary_to_list(Bin), both, $\n).

for_each_line_in_file(Name, Proc, Mode, Accum0) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Proc, Accum0).

for_each_line(Device, Proc, Accum) ->
    case io:get_line(Device, "") of
        eof  ->
            file:close(Device), Accum;
        Line ->
            NewAccum = Proc(Line, Accum),
            for_each_line(Device, Proc, NewAccum)
    end.

single_xors(Data, Char) ->
    F = fun(A) -> A bxor Char end,
    lists:map(F, Data).

rotate_char([A|Rest]) ->
    Rest ++ [A].

repeating_xors(D1, D2) ->
    repeating_xors(D1, D2, []).
repeating_xors([], _, Acc) ->
    lists:reverse(Acc);
repeating_xors([I1|R1], [I2|R2], Acc) ->
    repeating_xors(R1, R2 ++ [I2], [I1 bxor I2 | Acc]).

symmetric_xors(D1, D2) ->
    symmetric_xors(D1, D2, []).
symmetric_xors([], [], Acc) ->
    lists:reverse(Acc);
symmetric_xors([I1|R1], [I2|R2], Acc) ->
    symmetric_xors(R1, R2, [I1 bxor I2 | Acc]).


-ifdef(TEST).

hamming_distance_test() ->
    E = 37,
    R = hamming_distance(<<"this is a test">>, <<"wokka wokka!!!">>),
    ?assertEqual(E,R).

-endif.
