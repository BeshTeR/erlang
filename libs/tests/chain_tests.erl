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
    {[?chain(1,[],[])], "1"},
    {[?chain(2,[5,12],[])], "2+1/5+1/12"},
    {[?chain(3,[5,12],[2,3,4])], "3+1/5+1/12+{1/2+1/3+1/4}"}
    ];

tests(make, 1) ->
    [
    {[3], ?chain(3, [], [])},
    {[0], {error, bad_format}},
    {[atom], {error, bad_format}}
    ];

tests(make, 2) ->
    [
    {[1,[2,3]], ?chain(1,[2,3],[])},
    {[0.3,[2,3]], {error, bad_format}},
    {[3, {2,3}], {error, bad_format}}
    ];

tests(make, 3) ->
    [
    {[1,[2,3],[4,5,6,7]], ?chain(1,[2,3],[4,5,6,7])},
    {[-1,[2,3],[4,5,6,7]], {error, bad_format}},
    {[1,[atom,3],[4,5,6,7]], {error, bad_format}},
    {[1,[2,3],[4,5,6,0]], {error, bad_format}}
    ];

tests(_, _) ->
    [].
