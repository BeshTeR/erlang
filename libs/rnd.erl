%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы со случайными величинами
%%% @end
%%% ----------------------------------------------------------------------------
-module(rnd).

%% API
-export([new/1, new/2, elem/1, list/2, digit/0, num/1, bool/0, bit/0, byte/0, bitstring/1]).

%% Tests
-include("tests/rnd_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Случайное натуральное число не превосходящее заданного
%% @end
%% -----------------------------------------------------------------------------
-spec new(N) -> Result when
    N      :: non_neg_integer(),
    Result :: non_neg_integer().

new(N) when is_integer(N), N >= 0 ->
    round(N*rand:uniform()).

%% -----------------------------------------------------------------------------
%% @doc Случайное целое число из заданного диапазона
%% @end
%% -----------------------------------------------------------------------------
-spec new(M, N) -> Result when
    M      :: integer(),
    N      :: integer(),
    Result :: integer().

new(M, N) when is_integer(M), is_integer(N), M =< N ->
    round((N-M)*rand:uniform()+M).

%% -----------------------------------------------------------------------------
%% @doc Случайный элемент непустого списка
%% @end
%% -----------------------------------------------------------------------------
-spec elem(L) -> Result when
    L      :: [any()],
    Result :: any().

elem(L) when is_list(L), L =/= [] ->
    case L of
        [X] -> X;
        _   -> lists:nth(new(1, length(L)), L)
    end.

%% -----------------------------------------------------------------------------
%% @doc Список M случайных чисел, из диапазона 0 .. N
%% @end
%% -----------------------------------------------------------------------------
-spec list(M, N) -> Result when
    M      :: non_neg_integer(),
    N      :: non_neg_integer(),
    Result :: [pos_integer()].

list(M, N) when is_integer(M), is_integer(N), M >= 0, N >= 0 ->
    [new(N) || _ <- lists:seq(1, M)].

%% -----------------------------------------------------------------------------
%% @doc Случайное K-значное число
%% @end
%% -----------------------------------------------------------------------------
-spec num(K) -> Result when
    K      :: pos_integer(),
    Result :: non_neg_integer().

num(1) ->
    digit();
num(K) when is_integer(K), K > 1 ->
    lists:foldl(
        fun(A, B) -> A+10*B end,
        elem(lists:seq(1,9)),
        [digit() || _ <- lists:seq(1,K-1)]).

%% -----------------------------------------------------------------------------
%% @doc Случайная десятичная цифра
%% @end
%% -----------------------------------------------------------------------------
-spec digit() -> Result when
    Result :: non_neg_integer().

digit() ->
    trunc(10*rand:uniform()).

%% -----------------------------------------------------------------------------
%% @doc Случайное булево значение
%% @end
%% -----------------------------------------------------------------------------
-spec bool() -> Result when
    Result :: boolean().

bool() ->
    rand:uniform() > 0.5.

%% -----------------------------------------------------------------------------
%% @doc Случайный бит
%% @end
%% -----------------------------------------------------------------------------
-spec bit() -> Result when
    Result :: 0 | 1.

bit() ->
    round(rand:uniform()).

%% -----------------------------------------------------------------------------
%% @doc Случайный байт
%% @end
%% -----------------------------------------------------------------------------
-spec byte() -> Result when
    Result :: pos_integer().

byte() ->
    new(255).

%% -----------------------------------------------------------------------------
%% @doc Случайная битовая строка длиной N байт
%% @end
%% -----------------------------------------------------------------------------
-spec bitstring(N) -> Result when
    N      :: pos_integer(),
    Result :: bitstring().

bitstring(N) ->
   << <<(byte())>> || _ <- lists:seq(1, N)>>.
