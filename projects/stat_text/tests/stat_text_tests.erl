-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля stat_text
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(make, 1) ->
    [
    {["poem.txt"], ok}
    ];

tests(_, _) ->
    [].
