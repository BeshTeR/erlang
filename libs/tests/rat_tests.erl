-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля rat
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

%% Конструктор рационального числа для построения тестов
-define(R(N,D), rat:make(N,D)).

tests(make, 2) ->
    [
    {[0,5],    {0,1}},
    {[-2,-7],  {2,7}},
    {[33,-11], {-3,1}},
    {[24,80],  {3,10}},
    {[80,24],  {10,3}},
    {[15, 15], {1,1}},
    {[-7,7],   {-1,1}},
    {[0,-3],   {0,1}}
    ];

tests(numerator, 1) ->
    [
    {[?R(-4,5)], -4},
    {[?R(3,1)],  3},
    {[?R(0,1)],  0}
    ];

tests(denominator, 1) ->
    [
    {[?R(-4,5)], 5},
    {[?R(3,1)],  1},
    {[?R(0,1)],  1}
    ];

tests(zero, 0) ->
    [
    {[], ?R(0,1)}
    ];

tests(one, 0) ->
    [
    {[], ?R(1,1)}
    ];

tests(mult, 2) ->
    [
    {[?R(4,-2), ?R(-2,4)],  ?R(1,1)},
    {[?R(-7,12), ?R(3,2)],  ?R(-7,8)},
    {[?R(7,18), ?R(-9,14)], ?R(-1,4)},
    {[?R(0,1), ?R(12,35)],  ?R(0,1)}
    ];

tests(division, 2) ->
    [
    {[?R(4,-2), ?R(-2,4)],  ?R(4,1)},
    {[?R(-7,12), ?R(3,2)],  ?R(-7,18)},
    {[?R(7,18), ?R(-9,14)], ?R(-49,81)},
    {[?R(0,1), ?R(12,35)],  ?R(0,1)}
    ];

tests(rev, 1) ->
    [
    {[?R(2,3)],     ?R(3,2)},
    {[?R(-4,12)],   ?R(-3,1)},
    {[?R(-12,-15)], ?R(5,4)}
    ];

tests(add, 2) ->
    [
    {[?R(2,3), ?R(2,3)],   ?R(4,3)},
    {[?R(-3,4), ?R(3,4)],  ?R(0,1)},
    {[?R(1,7), ?R(2,5)],   ?R(19,35)},
    {[?R(0,1), ?R(-2,3)],  ?R(-2,3)},
    {[?R(12,4), ?R(10,5)], ?R(5,1)}
    ];

tests(sub, 2) ->
    [
    {[?R(2,3), ?R(2,3)],   ?R(0,1)},
    {[?R(-3,4), ?R(3,4)],  ?R(-3,2)},
    {[?R(1,7), ?R(2,5)],   ?R(-9,35)},
    {[?R(0,1), ?R(-2,3)],  ?R(2,3)},
    {[?R(12,4), ?R(10,5)], ?R(1,1)}
    ];

tests(pow, 2) ->
    [
    {[?R(2,1), 10],   ?R(1024,1)},
    {[?R(-2,4), 10],  ?R(1,1024)},
    {[?R(-3,7), 0],   ?R(1,1)},
    {[?R(-6,18), -1], ?R(-3,1)},
    {[?R(0,1), 0],    ?R(1,1)},
    {[?R(-3,7), -2],  ?R(49,9)}
    ];

tests(is_rational, 1) ->
    [
    {[?R(2,4)],  true},
    {[123],      false},
    {[{0,0}],    false},
    {[?R(-2,1)], true},
    {[{2,-1}],   false},
    {[{2}],      false},
    {[[2,3]],    false},
    {[?R(2,3)],  true}
    ];

tests(is_zero, 1) ->
    [
    {[?R(0,1)],  true},
    {[?R(0,-7)], true},
    {[?R(-0,2)], true},
    {[?R(2,3)],  false}
    ];

tests(is_negativ, 1) ->
    [
    {[?R(0,1)],   false},
    {[?R(0,-7)],  false},
    {[?R(-0,2)],  false},
    {[?R(2,3)],   false},
    {[?R(-7,5)],  true},
    {[?R(-7,-5)], false},
    {[?R(7,-5)],  true}
    ];

tests(is_positiv, 1) ->
    [
    {[?R(0,1)],   false},
    {[?R(0,-7)],  false},
    {[?R(-0,2)],  false},
    {[?R(2,3)],   true},
    {[?R(-7,5)],  false},
    {[?R(-7,-5)], true},
    {[?R(7,-5)],  false}
    ];

tests(equal, 2) ->
    [
    {[?R(2,4), ?R(-2,-4)],   true},
    {[?R(2,3), ?R(2,-3)],    false},
    {[?R(-2,3), ?R(2,-3)],   true},
    {[?R(18,12), ?R(15,10)], true},
    {[?R(0,1), ?R(0,-7)],    true}
    ];

tests(less, 2) ->
    [
    {[?R(2,4), ?R(-2,-4)],   false},
    {[?R(2,3), ?R(-2,3)],    false},
    {[?R(-2,3), ?R(2,-3)],   false},
    {[?R(18,12), ?R(21,14)], false},
    {[?R(0,1), ?R(0,-7)],    false},
    {[?R(2,4), ?R(2,3)],     true},
    {[?R(-1,3), ?R(1,7)],    true}
    ];

tests(greq, 2) ->
    [
    {[?R(2,4), ?R(-2,-4)], false},
    {[?R(2,3), ?R(-2,3)],  true},
    {[?R(-2,3), ?R(2,-3)], false},
    {[?R(18,12), ?R(3,2)], false},
    {[?R(0,1), ?R(0,-7)],  false},
    {[?R(2,4), ?R(2,3)],   false},
    {[?R(2,3), ?R(2,4)],   true},
    {[?R(-1,3), ?R(1,7)],  false}
    ];

tests(to_string, 1) ->
    [
    {[?R(5,18)],   "5/18"},
    {[?R(-15,19)], "-15/19"},
    {[?R(0,3)],    "0"},
    {[?R(2,1)],    "2"},
    {[?R(-4,1)],   "-4"}
    ];

tests(to_float, 1) ->
    [
    {[?R(5,18)],   5/18},
    {[?R(-15,19)], -15/19},
    {[?R(0,1)],    0.0},
    {[?R(2,1)],    2.0},
    {[?R(-4,1)],   -4.0}
    ];

tests(split, 1) ->
    [
    {[?R(5,18)],   {0, ?R(5,18)}},
    {[?R(-15,19)], {0, ?R(-15,19)}},
    {[?R(0,4)],    {0, ?R(0,1)}},
    {[?R(5,2)],    {2, ?R(1,2)}},
    {[?R(-20,7)],  {-2, ?R(-6,7)}}
    ];

tests(integer_to_rat, 1) ->
    [
    {[1],  ?R(1,1)},
    {[0],  ?R(0,1)},
    {[5],  ?R(5,1)},
    {[-8], ?R(-8,1)}
    ];

tests(float_to_rat, 2) ->
    [
    {[1.5, 1],  ?R(3,2)},
    {[0.0, 1],  ?R(0,1)},
    {[1.0, 1],  ?R(1,1)},
    {[8.0, 10], ?R(8,1)},
    {[0.25, 2], ?R(1,4)},
    {[2.5, 3],  ?R(5,2)}
    ];

tests(is_full, 1) ->
    [
    {[?R(5,18)], false},
    {[?R(18,5)], false},
    {[?R(18,9)], true},
    {[?R(9,18)], false},
    {[?R(0,1)],  true},
    {[?R(-2,1)], true},
    {[?R(-4,2)], true}
    ];

tests(_, _) ->
    [].
