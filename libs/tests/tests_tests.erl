-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля tests
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(fast, 3) ->
    [
    {[[], [2, 1000], 100_000],                        {error, no_functions}},
    {[[{lib, pow}], [2, 1000], 100_000],              {error, one_function}},
    {[[{math, pow}, {lib, pow}], [2, 1000], 100_000], [ok, ok]}
    ];

tests(_, _) ->
    [].
