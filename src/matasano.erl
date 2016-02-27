-module(matasano).

%% API
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

s1_c1(Data) ->
    binary_to_list(
      base64:encode(
        hex2s(Data)
       )
     ).

s1_c2(Data1, Data2) ->
   s2hex(
     symmetric_xors(
       hex2s(Data1),
       hex2s(Data2))
    ).

s1_c3(Data) ->
    PossibleChars = lists:seq(1,256),
    decrypt_single_xors(Data, PossibleChars, 0, 0, "").

s1_c4(Filename) ->
    PossibleChars = lists:seq(1,256),
    F = fun(Entry, {_, TopScore, TopString}) ->
                E = string:strip(Entry, both, $\n),
                {TC, TSc, TSt} = decrypt_single_xors(E, PossibleChars, 0, TopScore, TopString),
                {TC, TSc, TSt}
        end,
    for_each_line_in_file(Filename, F, [read], {0, 0, ""}).

s1_c5(Filename, Key) ->
    String = file_to_string(Filename),
    s2hex(repeating_xors(String, Key)).

s1_c6(Filename) ->
    String = file_to_string(Filename),
    Data = binary_to_list(base64:decode(String)),
    decrypt_repeating_xors(Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

decrypt_single_xors(_Data, [], TopChar, TopScore, TopString) ->
    {TopChar, TopScore, TopString};
decrypt_single_xors(Data, [C | Rest], TopChar, TopScore, TopString) ->
    String = single_xors(
               hex2s(Data),
               C
              ),
    Score = analyze_english:score(String),
    case Score > TopScore of
        true ->
            decrypt_single_xors(Data, Rest, C, Score, String);
        _ ->
            decrypt_single_xors(Data, Rest, TopChar, TopScore, TopString)
    end.

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
    PossibleChars = lists:seq(1,256),
    {Char, _, _} = decrypt_single_xors(s2hex(T), PossibleChars, 0, 0, ""),
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

file_to_string(Filename) ->
    F = fun(Entry, Acc) -> [Entry|Acc] end,
    R = for_each_line_in_file(Filename, F, [read], []),
    string:strip(lists:flatten(lists:reverse(R)), both, $\n).

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

hex2s(Data) ->
    hex2s(Data, []).
hex2s([X1,X2|Rest], Acc) ->
    hex2s(Rest, [ list_to_integer([X1,X2], 16) |Acc ]);
hex2s([], Acc) -> lists:reverse(Acc).

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

s2hex(0) ->
    "0";
s2hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
s2hex(B) ->
    to_hex(iolist_to_binary(B), []).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

s1_c1_test() ->
    E = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t",
    R = s1_c1("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"),
    ?assertEqual(E, R).

s1_c2_test() ->
    E = "746865206b696420646f6e277420706c6179",
    R = s1_c2("1c0111001f010100061a024b53535009181c","686974207468652062756c6c277320657965"),
    ?assertEqual(E, R).

s1_c3_test() ->
    E = {88, 36, "Cooking MC's like a pound of bacon"},
    R = s1_c3("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"),
    ?assertEqual(E, R).

s1_c4_test() ->
    E = {0, 47,"Now that the party is jumping\n"},
    R = s1_c4("../priv/s1_c4.txt"),
    ?assertEqual(E,R).

s1_c5_test() ->
    E = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f",
    R = s1_c5("../priv/s1_c5.txt", "ICE"),
    ?assertEqual(E,R).

hamming_distance_test() ->
    E = 37,
    R = hamming_distance(<<"this is a test">>, <<"wokka wokka!!!">>),
    ?assertEqual(E,R).

s1_c6_test() ->
    E = "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n",
    R = s1_c6("../priv/s1_c6.txt"),
    ?assertEqual(E,R).

-endif.
