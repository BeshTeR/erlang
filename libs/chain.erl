%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с цепными дробями (... В РАЗРАБОТКЕ)
%%% @end
%%% ----------------------------------------------------------------------------
-module(chain).

%% API -------------------------------------------------------------------------

%% Конструкторы
-export([make/1, make/2, make/3]).

%% Предикаты
-export([is_chain/1]).

%% Математические операции
%% ...

%% Логические операции сравнения
%% ...

%% Преобразования
-export([to_string/1, to_rat/1, to_float/1, from_rat/1, from_float/1]).

%% Разное
-export([depth/1, depth/2, nth/2]).

%% Tests -----------------------------------------------------------------------
-include("tests/chain_tests.erl").

%% Тип - цепная дробь ----------------------------------------------------------
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

%% -----------------------------------------------------------------------------
%% @doc Преобразовать цепную дробь в рациональной число
%% @end
%% -----------------------------------------------------------------------------
-spec to_rat(C) -> Return when
    C      :: chain(),
    Return :: rat:rat().

to_rat(C) -> ok.

%% -----------------------------------------------------------------------------
%% @doc Преобразовать цепную дробь в вещественное число
%% @end
%% -----------------------------------------------------------------------------
-spec to_float(C) -> Return when
    C      :: chain(),
    Return :: float().

to_float(C) -> rat:to_float(to_rat(C)).

%% -----------------------------------------------------------------------------
%% @doc Преобразовать рациональное число в цепную дробь
%% @end
%% -----------------------------------------------------------------------------
-spec from_rat(R) -> Return when
    R      :: rat:rat(),
    Return :: chain().

from_rat(C) -> ok.

%% -----------------------------------------------------------------------------
%% @doc Преобразовать вещественное число в цепную дробь
%% @end
%% -----------------------------------------------------------------------------
-spec from_float(X) -> Return when
    X      :: float(),
    Return :: chain().

from_float(X) -> from_rat(rat:from_float(X)).

%% -----------------------------------------------------------------------------
%% @doc Глубина цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec depth(C) -> Return when
    C      :: chain(),
    Return :: non_neg_integer() | infinity.

depth({_, [], []}) -> 0;
depth({_, L1, []}) -> length(L1);
depth({_, _, _}) -> infinity.

%% -----------------------------------------------------------------------------
%% @doc Глубина цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec depth(C, N) -> Return when
    C      :: chain(),
    N      :: non_neg_integer(),
    Return :: chain().

depth(C, N) when is_integer(N), N >= 0 -> ok.

%% -----------------------------------------------------------------------------
%% @doc N-й элемент цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec nth(N, C) -> Return when
    N      :: non_neg_integer(),
    C      :: chain(),
    Return :: pos_integer() | undefined.

nth(0, {M, _, _}) -> M;
nth(N, {_, L1, _}) when N =< length(L1) -> lists:nth(N, L1);
nth(N, {_, L1, L2}) -> nth(L2, N-length(L1));
nth(_, []) -> undefined;
nth(N, L) when N =< length(L) -> lists:nth(N, L);
nth(N, L) -> nth(N-length(L), L).
