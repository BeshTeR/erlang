-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля rpn
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(eval, 1) ->
    [
    {["2 3 +"], 5},
    {["11.9 2.10 - 5 *"], 49.0},
    {["0 sin"], 0.0},
    {["2 10 ^"], 1024},
    {["-10.0 -2 ^"], 0.01},
    {["23 4 div"], 5},
    {["23 4 rem"], 3},
    {["4 sqrt 3 ^ -1.0 -"], 9.0},
    {["1 tg atg"], 1.0},
    {["0 cos acos"], 0.0},
    {["1 asin sin"], 1.0},
    {["3 exp ln"], 3.0},
    {["10 log -0.3 +"], 0.7},
    {["1.0 actg"], 0.7853981633974483},
    {["pi"], math:pi()},
    {["6 !"], 720},
    {["1 2 3 4 5 6 7 8 9 10 sum"], 55},
    {["1 2 3 4 5 6 7 8 9 10 prod 10 ! -"], 0},
    {["1 2 3 4 sum 5 6 7 8 9 10 sum"], 55},
    {["true"], true},
    {["false"], false},
    {["true false true &"], false},
    {["true false true v"], true},
    {["true false not true &"], true},
    {["true false true xor"], true},
    {["true not false true not xor"], true},
    {["true false not true xor"], false},
    {["120 5 ! ="], true},
    {["1 2 <"], true},
    {["1 2 =<"], true},
    {["1 2 >"], false},
    {["1 2 >="], false},
    {["2 2 =<"], true},
    {["2 2 >="], true},
    {["2 2 /= not"], true},
    {["2.6 trunc"], 2},
    {["-2.4 trunc"], -2},
    {["2.6 round"], 3},
    {["-2.4 round"], -2},
    {["true false and"], false},
    {["true false or not not"], true},
    {["2 2.0 =="], false},
    {["2 2.0 ="], true},
    {["2 2.0 /="], false},
    {["2 2.0 =/= not"], false}
    ];

tests(_, _) ->
    [].
