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
    s1_c3(Data, PossibleChars, 0, "").

s1_c3(_Data, [], TopScore, TopString) ->
    {TopScore, TopString};
s1_c3(Data, [C | Rest], TopScore, TopString) ->
    String = single_xors(
               hex2s(Data),
               C
              ),
    Score = analyze_english:score(String),
    case Score > TopScore of
        true ->
            s1_c3(Data, Rest, Score, String);
        _ ->
            s1_c3(Data, Rest, TopScore, TopString)
    end.

s1_c4(Filename) ->
    PossibleChars = lists:seq(1,256),
    F = fun(Entry, {TopScore, TopString}) ->
                E = string:strip(Entry, both, $\n),
                s1_c3(E, PossibleChars, TopScore, TopString)
        end,
    for_each_line_in_file(Filename, F, [read], {0, ""}).

s1_c5(Filename, Key) ->
    F = fun(Entry, Acc) ->
                E0 = string:strip(Entry, left, $\n),
                %% E = s2hex(E0),
                %% [repeating_xors(Entry, Key)++ [$\n]|Acc]
                [E0|Acc]
        end,
    R = for_each_line_in_file(Filename, F, [read], []),
    s2hex(repeating_xors(lists:flatten(lists:reverse(R)), Key)).
    %% s2hex(lists:reverse(R)).
    %% lists:reverse(R).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
    E = {36, "Cooking MC's like a pound of bacon"},
    R = s1_c3("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"),
    ?assertEqual(E, R).

s1_c4_test() ->
    E = {47,"Now that the party is jumping\n"},
    R = s1_c4("../priv/s1_c4.txt"),
    ?assertEqual(E,R).

s1_c5_test() ->
    E = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f",
    R = s1_c5("../priv/s1_c5.txt", "ICE"),
    ?assertEqual(E,R).

-endif.
