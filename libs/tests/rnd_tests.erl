-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля rnd
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(new, 2) ->
    [
    {[0,0], 0},
    {[-1,-1], -1},
    {[10,10], 10}
    ];

tests(elem, 1) ->
    [
    {[[atom]], atom},
    {[[12]], 12},
    {[[{1,2}]], {1,2}}
    ];

tests(list, 2) ->
    [
    {[0,1],  []},
    {[10,1], [1,1,1,1,1,1,1,1,1,1]}
    ];

tests(_, _) ->
    [].
