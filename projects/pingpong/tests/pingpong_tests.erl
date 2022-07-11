-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля pingpong
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(start1, 1) ->
    [
    {[5], ok}
    ];

tests(start2, 1) ->
    [
    {[0], ok},
    {[5], ok}
    ];

tests(_, _) ->
    [].
