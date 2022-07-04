-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля tests
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(_, _) ->
    [].
