-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля chain
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

%% Конструктор цепной дроби
-define(chain(N,L1,L2), chain:make(N,L1,L2)).

tests(to_string, 1) ->
    [
    {[?chain(1,[],[])],          "1"},
    {[?chain(2,[5,12],[])],      "2+1/5+1/12"},
    {[?chain(3,[5,12],[2,3,4])], "3+1/5+1/12+{1/2+1/3+1/4}"}
    ];

tests(make, 1) ->
    [
    {[3],    ?chain(3, [], [])},
    {[0],    {error, bad_format}},
    {[atom], {error, bad_format}}
    ];

tests(make, 2) ->
    [
    {[1,[2,3]],   ?chain(1,[2,3],[])},
    {[0.3,[2,3]], {error, bad_format}},
    {[3, {2,3}],  {error, bad_format}}
    ];

tests(make, 3) ->
    [
    {[1,[2,3],[4,5,6,7]],    ?chain(1,[2,3],[4,5,6,7])},
    {[1,[2,3],[]],           ?chain(1,[2,3],[])},
    {[1,[],[4,5,6,7]],       ?chain(1,[],[4,5,6,7])},
    {[1,[],[]],              ?chain(1,[],[])},
    {[-1,[2,3],[4,5,6,7]],   {error, bad_format}},
    {[1,[atom,3],[4,5,6,7]], {error, bad_format}},
    {[1,[2,3],[4,5,6,0]],    {error, bad_format}}
    ];

tests(is_chain, 1) ->
    [
    {[{1,[],[]}],              true},
    {[{2,[5,12],[]}],          true},
    {[{3,[5,12],[2,3,4]}],     true},
    {[{3,[],[2,3,4]}],         true},
    {[{-1,[2,3],[4,5,6,7]}],   false},
    {[{1,[atom,3],[4,5,6,7]}], false},
    {[{1,[2,3],[4,5,6,0]}],    false},
    {[{1,[0,3],[4,5,6,2]}],    false},
    {[{1,[-2,3],[4,5,6,2]}],   false},
    {[{0,[2,3],[4,5,6,2]}],    false},
    {[{2,[2,3],[4,atom,6,2]}], false},
    {[{atom,[2,3],[4,5,6,2]}], false},
    {[{1,[2,3]}],              false},
    {[{1}],                    false},
    {[atom],                   false}
    ];

tests(to_rat, 1) ->
    [
    ];

tests(to_float, 1) ->
    [
    ];

tests(from_rat, 1) ->
    [
    ];

tests(from_float, 1) ->
    [
    ];

tests(depth, 1) ->
    [
    {[?chain(3, [], [])],        0},
    {[?chain(3, [22], [])],      1},
    {[?chain(3, [1, 2], [])],    2},
    {[?chain(3, [], [1])],       infinity},
    {[?chain(3, [2,3], [1, 2])], infinity}
    ];

tests(depth, 2) ->
    [
    ];

tests(nth, 2) ->
    [
    ];

tests(_, _) ->
    [].
