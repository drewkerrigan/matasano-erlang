-module(analyze_english).

-export([simple_score/1, score/1, score/2]).

%%%===================================================================
%%% API
%%%===================================================================

simple_score(String) ->
    U = string:to_upper(String),
    lists:foldl(fun(X, Sum) -> simple_letter_score([X]) + Sum end, 0, U).

score(String) ->
    score(String, 0.7).

score(String, Threshold) ->
    U = string:to_upper(String),
    NumLetters = lists:foldl(fun(X, Sum) -> any_letter([X]) + Sum end, 0, U),
    case (NumLetters / length(String)) of
        X when X >= Threshold ->
            Words = string:tokens(U, "'., "),
            S0 = score_words(Words),
            S1 = score_letters(lists:flatten(Words)),
            round(S0 + S1);
        _ -> 0
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

score_letters(Letters) ->
    score_letters(Letters, 0.0).

score_letters([], Score) ->
    Score;
score_letters([L1], Score) ->
    Score + letter_percentage(L1);
score_letters([L1, L2], Score) ->
    S0 = digraph([L1, L2]),
    S1 = letter_percentage([L1]),
    Score + S0 + S1;
score_letters([69, L1 | Rest], Score) ->
    S0 = after_e([L1]),
    S1 = letter_percentage([69]),
    score_letters([L1 | Rest], Score + S0 + S1);
score_letters([L1, L1 | Rest], Score) ->
    S0 = double([L1, L1]),
    S1 = letter_percentage([L1]),
    score_letters([L1 | Rest], Score + S0 + S1);
score_letters([L1, L2, L3 | Rest], Score) ->
    S0 = digraph([L1, L2]),
    S1 = digraph([L2, L3]),
    S2 = trigraph([L1, L2, L3]),
    S3 = letter_percentage([L1]),
    score_letters([L2, L3 | Rest], Score + S0 + S1 + S2 + S3).

score_words(Words) ->
    score_words(Words, 0).

score_words([], Score) ->
    Score;
score_words([Word|Rest], Score) ->
    New = score_word(Word),
    score_words(Rest, Score + New).

score_word(Word) ->
    S0 = common_word(Word),

    S1 = case Word of
        [_] ->
                 one_letter_word(Word);
        [L1,L2] ->
                 two_letter_word(Word) +
                     first_letter([L1]) +
                     second_letter([L2]);
        [L1,L2,L3] ->
                 three_letter_word(Word) +
                     first_letter([L1]) +
                     second_letter([L2]) +
                     third_letter([L3]);
        [L1,L2,L3,L4]->
                 four_letter_word(Word) +
                     first_letter([L1]) +
                     second_letter([L2]) +
                     third_letter([L3]) +
                     last_letter([L4]);
        [L1,L2,L3,_L4 | _] ->
                 first_letter([L1]) +
                     second_letter([L2]) +
                     third_letter([L3]) +
                     last_letter(lists:nthtail(length(Word)-1, Word))
         end,
    S0 + S1.

any_letter([L]) ->
    case lists:member(L, "ABCDEFGHIJKLMNOPQRSTUVWXYZ ") of
        true ->
            1;
        _ ->
            0
    end.

simple_letter_score([L]) ->
    case lists:member(L, "ETAOIN SHRDLU") of
        true ->
            1;
        _ ->
            0
    end.

letter_percentage("E") -> 1.0;
letter_percentage("T") -> 9.25 / 12.51;
letter_percentage("A") -> 8.04 / 12.51;
letter_percentage("O") -> 7.60 / 12.51;
letter_percentage("I") -> 7.26 / 12.51;
letter_percentage("N") -> 7.09 / 12.51;
letter_percentage("S") -> 6.54 / 12.51;
letter_percentage("R") -> 6.12 / 12.51;
letter_percentage("H") -> 5.49 / 12.51;
letter_percentage("L") -> 4.14 / 12.51;
letter_percentage("D") -> 3.99 / 12.51;
letter_percentage("C") -> 3.06 / 12.51;
letter_percentage("U") -> 2.71 / 12.51;
letter_percentage("M") -> 2.53 / 12.51;
letter_percentage("F") -> 2.30 / 12.51;
letter_percentage("P") -> 2.00 / 12.51;
letter_percentage("G") -> 0.1;
letter_percentage("W") -> 0.1;
letter_percentage("Y") -> 0.1;
letter_percentage("B") -> 0.1;
letter_percentage("V") -> 0.1;
letter_percentage("K") -> 0.1;
letter_percentage("X") -> 0.1;
letter_percentage("J") -> 0.1;
letter_percentage("Q") -> 0.1;
letter_percentage("Z") -> 0.1;
letter_percentage(_) -> 0.0.

first_letter("T") -> 1.0;
first_letter("O") -> 0.9;
first_letter("A") -> 0.9;
first_letter("W") -> 0.9;
first_letter("B") -> 0.8;
first_letter("C") -> 0.8;
first_letter("D") -> 0.8;
first_letter("S") -> 0.7;
first_letter("F") -> 0.7;
first_letter("M") -> 0.7;
first_letter("R") -> 0.6;
first_letter("H") -> 0.6;
first_letter("I") -> 0.6;
first_letter("Y") -> 0.6;
first_letter("E") -> 0.6;
first_letter("G") -> 0.5;
first_letter("L") -> 0.5;
first_letter("N") -> 0.5;
first_letter("U") -> 0.5;
first_letter("J") -> 0.5;
first_letter("K") -> 0.5;
first_letter(_) -> 0.0.

second_letter("H") -> 1.0;
second_letter("O") -> 0.9;
second_letter("E") -> 0.9;
second_letter("I") -> 0.8;
second_letter("A") -> 0.8;
second_letter("U") -> 0.7;
second_letter("N") -> 0.7;
second_letter("R") -> 0.6;
second_letter("T") -> 0.5;
second_letter(_) -> 0.0.

third_letter("E") -> 1.0;
third_letter("S") -> 0.9;
third_letter("A") -> 0.8;
third_letter("R") -> 0.7;
third_letter("N") -> 0.6;
third_letter("I") -> 0.5;
third_letter(_) -> 0.0.

last_letter("E") -> 1.0;
last_letter("S") -> 1.0;
last_letter("T") -> 1.0;
last_letter("D") -> 1.0;
last_letter("N") -> 0.8;
last_letter("R") -> 0.8;
last_letter("Y") -> 0.8;
last_letter("F") -> 0.7;
last_letter("L") -> 0.7;
last_letter("O") -> 0.7;
last_letter("G") -> 0.6;
last_letter("H") -> 0.6;
last_letter("A") -> 0.6;
last_letter("K") -> 0.6;
last_letter("M") -> 0.6;
last_letter("P") -> 0.5;
last_letter("U") -> 0.5;
last_letter("W") -> 0.5;
last_letter(_) -> 0.0.

after_e("R") -> 1.0;
after_e("S") -> 0.9;
after_e("N") -> 0.8;
after_e("D") -> 0.7;
after_e(_) -> 0.0.

digraph("TH") -> 1.0;
digraph("HE") -> 0.9;
digraph("AN") -> 0.9;
digraph("IN") -> 0.9;
digraph("ER") -> 0.9;
digraph("ON") -> 0.9;
digraph("RE") -> 0.8;
digraph("ED") -> 0.8;
digraph("ND") -> 0.8;
digraph("HA") -> 0.8;
digraph("AT") -> 0.7;
digraph("EN") -> 0.7;
digraph("ES") -> 0.7;
digraph("OF") -> 0.7;
digraph("NT") -> 0.6;
digraph("EA") -> 0.6;
digraph("TI") -> 0.6;
digraph("TO") -> 0.6;
digraph("IO") -> 0.5;
digraph("LE") -> 0.5;
digraph("IS") -> 0.5;
digraph("OU") -> 0.5;
digraph("AR") -> 0.5;
digraph("AS") -> 0.5;
digraph("DE") -> 0.5;
digraph("RT") -> 0.5;
digraph("VE") -> 0.5;
digraph(_) -> 0.0.

trigraph("THE") -> 1.0;
trigraph("AND") -> 0.9;
trigraph("THA") -> 0.9;
trigraph("ENT") -> 0.9;
trigraph("ION") -> 0.8;
trigraph("TIO") -> 0.8;
trigraph("FOR") -> 0.7;
trigraph("NDE") -> 0.7;
trigraph("HAS") -> 0.6;
trigraph("NCE") -> 0.6;
trigraph("TIS") -> 0.5;
trigraph("OFT") -> 0.5;
trigraph("MEN") -> 0.5;
trigraph(_) -> 0.0.

double("SS") -> 1.0;
double("EE") -> 0.9;
double("TT") -> 0.8;
double("FF") -> 0.7;
double("LL") -> 0.6;
double("MM") -> 0.5;
double("OO") -> 0.5;
double(_) -> 0.0.

one_letter_word(Word) ->
    C = ["I", "A", "U"],
    case lists:member(Word, C) of
        true -> 1.0;
        _ -> 0.0
    end.

two_letter_word(Word) ->
    C = ["OF", "TO", "IN", "IT", "IS", "BE", "AS", "AT", "SO", "WE", "HE", "BY", "OR", "ON", "DO", "IF", "ME", "MY", "UP", "AN", "GO", "NO", "US", "AM"],
    case lists:member(Word, C) of
        true -> 1.0;
        _ -> 0.0
    end.

three_letter_word(Word) ->
    C = ["THE", "AND", "FOR", "ARE", "BUT", "NOT", "YOU", "ALL", "ANY", "CAN", "HAD", "HER", "WAS", "ONE", "OUR", "OUT", "DAY", "GET", "HAS", "HIM", "HIS", "HOW", "MAN", "NEW", "NOW", "OLD", "SEE", "TWO", "WAY", "WHO", "BOY", "DID", "ITS", "LET", "PUT", "SAY", "SHE", "TOO", "USE"],
    case lists:member(Word, C) of
        true -> 1.0;
        _ -> 0.0
    end.

four_letter_word(Word) ->
    C = ["THAT", "WITH", "HAVE", "THIS", "WILL", "YOUR", "FROM", "THEY", "KNOW", "WANT", "BEEN", "GOOD", "MUCH", "SOME", "TIME", "VERY", "WHEN", "COME", "HERE", "JUST", "LIKE", "LONG", "MAKE", "MANY", "MORE", "ONLY", "OVER", "SUCH", "TAKE", "THAN", "THEM", "WELL", "WERE"],
    case lists:member(Word, C) of
        true -> 1.0;
        _ -> 0.0
    end.

common_word(Word) ->
    C = ["THE", "OF", "AND", "TO", "IN", "A", "IS", "THAT", "BE", "IT", "BY", "ARE", "FOR", "WAS", "AS", "HE", "WITH", "ON", "HIS", "AT", "WHICH", "BUT", "FROM", "HAS", "THIS", "WILL", "ONE", "HAVE", "NOT", "WERE", "OR", "ALL", "THEIR", "AN", "I", "THERE", "BEEN", "MANY", "MORE", "SO", "WHEN", "HAD", "MAY", "TODAY", "WHO", "WOULD", "TIME", "WE", "ABOUT", "AFTER", "DOLLARS", "IF", "MY", "OTHER", "SOME", "THEM", "BEING", "ITS", "NO", "ONLY", "OVER", "VERY", "YOU", "INTO", "MOST", "THAN", "THEY", "DAY", "EVEN", "MADE", "OUT", "FIRST", "GREAT", "MUST", "THESE", "CAN", "DAYS", "EVERY", "FOUND", "GENERAL", "HER", "HERE", "LAST", "NEW", "NOW", "PEOPLE", "PUBLIC", "SAID", "SINCE", "STILL", "SUCH", "THROUGH", "UNDER", "UP", "WAR", "WELL", "WHERE", "WHILE", "YEARS", "BEFORE", "BETWEEN", "COUNTRY", "DEBTS", "GOOD", "HIM", "INTEREST", "LARGE", "LIKE", "MAKE", "OUR", "TAKE", "UPON", "WHAT"],
    case lists:member(Word, C) of
        true -> 1.0;
        _ -> 0.0
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

score_test() ->
    ?assertEqual(21, simple_score("hello there, how are you.")),
    ?assertEqual(41, score("hello there, how are you.")),
    ?assertEqual(0, score("lk23j4523h523h5234h6234kh6lk;asdfa")).

-endif.
