%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с цепными дробями (... В РАЗРАБОТКЕ)
%%% @end
%%% ----------------------------------------------------------------------------
-module(chain).

%% API
-export([make/1, make/2, make/3, is_chain/1, to_string/1]).

%% Tests
-include("tests/chain_tests.erl").

%% Тип - цепная дробь
-type chain() :: {pos_integer(),    % целая часть дроби
                 [pos_integer()],   % непереодическая часть дроби
                 [pos_integer()]}.  % периодическая часть дроби

%% -----------------------------------------------------------------------------
%% @doc Конструкторы цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec make(N) -> Return when
    N      :: pos_integer(),
    Return :: chain() | {error, bad_format}.

make(N) -> make(N, [], []).

-spec make(N, L1) -> Return when
    N      :: pos_integer(),
    L1     :: [pos_integer()],
    Return :: chain() | {error, bad_format}.

make(N, L1) -> make(N, L1, []).

-spec make(N, L1, L2) -> Return when
    N      :: pos_integer(),
    L1     :: [pos_integer()],
    L2     :: [pos_integer()],
    Return :: chain() | {error, bad_format}.

make(N, L1, L2) ->
    C = {N, L1, L2},
    case is_chain(C) of
        true -> C;
        false -> {error, bad_format}
    end.

%% -----------------------------------------------------------------------------
%% @doc Это цепная дробь ?
%% @end
%% -----------------------------------------------------------------------------
-spec is_chain(C) -> Return when
    C      :: any(),
    Return :: boolean().

is_chain({N, L1, L2}) when is_integer(N), N > 0 ->
    is_list_integer(L1) andalso is_list_integer(L2);
is_chain(_) -> false.

%% это список целых положительных чисел ?
is_list_integer([]) -> true;
is_list_integer([H|T]) when is_integer(H), H > 0 -> is_list_integer(T);
is_list_integer(_) -> false.

%% -----------------------------------------------------------------------------
%% @doc Преобразовать цепную дробь в строку
%% @end
%% -----------------------------------------------------------------------------
-spec to_string(C) -> Return when
    C      :: chain(),
    Return :: string().

to_string({N, L1, L2}) ->
    integer_to_list(N) ++
    case L1 =:= [] of
        true -> "";
        false ->"+" ++ to_string(L1)
    end ++
    case L2 =:= [] of
        true -> "";
        false -> "+{" ++ to_string(L2) ++ "}"
    end;
to_string(L) -> to_string(L, "").

to_string([], Str) -> Str;
to_string([H], Str) ->
    Str ++ "1/" ++ integer_to_list(H);
to_string([H|L], Str) ->
    to_string(L, Str ++ "1/" ++ integer_to_list(H) ++ "+").
