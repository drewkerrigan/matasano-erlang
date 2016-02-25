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
     xors(
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

hex2s(Data) ->
    hex2s(Data, []).
hex2s([X1,X2|Rest], Acc) ->
    hex2s(Rest, [ list_to_integer([X1,X2], 16) |Acc ]);
hex2s([], Acc) -> lists:reverse(Acc).

single_xors(Data, Char) ->
    F = fun(A) -> A bxor Char end,
    lists:map(F, Data).

xors(D1, D2) ->
    xors(D1, D2, []).
xors([], [], Acc) ->
    lists:reverse(Acc);
xors([I1|R1], [I2|R2], Acc) ->
    xors(R1, R2, [I1 bxor I2 | Acc]).

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
    E = {37, "Cooking MC's like a pound of bacon"},
    R = s1_c3("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"),
    ?assertEqual(E, R).

-endif.
