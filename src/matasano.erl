-module(matasano).

%% API
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================

s1_c1(Data) ->
    binary_to_list(
      base64:encode(
        mochihex:to_bin(Data)
       )
     ).

s1_c2(Data1, Data2) ->
   mochihex:to_hex(
     crypto_util:symmetric_xors(
       binary_to_list(mochihex:to_bin(Data1)),
       binary_to_list(mochihex:to_bin(Data2)))
    ).

s1_c3(Data) ->
    Unhex = mochihex:to_bin(Data),
    UnhexStr = binary_to_list(Unhex),
    crypto_util:decrypt_single_xors(UnhexStr).

s1_c4(Filename) ->
    Lines = crypto_util:file_as_lines(Filename),
    lists:foldl(fun(X, {C,Sc,St}) ->
                        Entry1 = string:strip(X, both, $\n),
                        Unhex = mochihex:to_bin(Entry1),
                        UnhexStr = binary_to_list(Unhex),
                        case crypto_util:decrypt_single_xors(UnhexStr) of
                            {Char, Score, String} when Score > Sc -> {Char, Score, String};
                            _ -> {C, Sc, St}
                        end
                end, {0,0,""}, Lines).

s1_c5(Filename, Key) ->
    String = crypto_util:file_to_string(Filename),
    mochihex:to_hex(crypto_util:repeating_xors(String, Key)).

s1_c6(Filename) ->
    String = crypto_util:file_to_string(Filename),
    Data = binary_to_list(base64:decode(String)),
    crypto_util:decrypt_repeating_xors(Data).

s1_c7(Filename) ->
    Base64 = crypto_util:file_to_string(Filename),
    Data = base64:decode(Base64),
    crypto_util:aes_128_ecb_decrypt(Data, "YELLOW SUBMARINE").

s1_c8(Filename) ->
    Lines = crypto_util:file_as_lines(Filename),
    lists:foldl(fun(X, Accum) ->
                        Entry1 = string:strip(X, both, $\n),
                        Unhex = mochihex:to_bin(Entry1),
                        UnhexStr = binary_to_list(Unhex),
                        case crypto_util:is_aes_128_ecb(UnhexStr) of
                            true -> {true, X};
                            _ -> Accum
                        end
                end, {0,0,""}, Lines).

s2_c9(Block, Length) ->
    crypto_util:pkcs7_pad(Block, Length).


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(MUSIC, "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n").

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
    E = {53, 47,"Now that the party is jumping\n"},
    R = s1_c4("../priv/s1_c4.txt"),
    ?assertEqual(E,R).

s1_c5_test() ->
    E = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f",
    R = s1_c5("../priv/s1_c5.txt", "ICE"),
    ?assertEqual(E,R).

s1_c6_test() ->
    E = ?MUSIC,
    R = s1_c6("../priv/s1_c6.txt"),
    ?assertEqual(E,R).

s1_c7_test() ->
    E = ?MUSIC,
    R = s1_c7("../priv/s1_c7.txt"),
    ?assertEqual(E,R).

s1_c8_test() ->
    E = {true, "d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd2839475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd28397a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a\n"},
    R = s1_c8("../priv/s1_c8.txt"),
    ?assertEqual(E,R).

s2_c9_test() ->
    E = "YELLOW SUBMARINE\x04\x04\x04\x04",
    R = s2_c9("YELLOW SUBMARINE", 20),
    ?assertEqual(E,R).
-endif.
