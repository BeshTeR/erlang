%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека некоторых часто используемых функций
%%% @end
%%% ----------------------------------------------------------------------------
-module(lib).

%% API
-export([gcd/2, sign/1, pow/2, fac/1, id/1, map/2, filter/2, flush/0, sleep/1, on_exit/2, for/4, for/3, for/2]).

%% Tests
-include("tests/lib_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Быстрое возведение числа в степень по инвариантам:
%% N^M = (N^(M/2))^2 - если M четно
%% N^M = N*(N^(M-1)) - если M нечетно
%% @end
%% -----------------------------------------------------------------------------
-spec pow(N, M) -> Return when
    N      :: number(),
    M      :: integer(),
    Return :: number().

pow(N, M) when is_number(N), is_integer(M) ->
    case M >= 0 of
        true  -> pow(N, M, 1);
        false -> 1/pow(N, -M, 1)
    end.

% тело pow с аккумулятором
pow(_, 0, Res) ->
    Res;
pow(N, M, Res) when M rem 2 =:= 0 ->
    pow(N*N, M div 2, Res);
pow(N, M, Res) ->
    pow(N, M-1, N*Res).

%% -----------------------------------------------------------------------------
%% @doc Факториал числа
%% @end
%% -----------------------------------------------------------------------------
-spec fac(N) -> Return when
    N      :: non_neg_integer(),
    Return :: pos_integer().

fac(N) when is_integer(N), N >=0 ->
    fac(N, 1).

%% тело fac с аккумулятором
fac(0, Res) ->
    Res;
fac(N, Res) ->
    fac(N-1, N*Res).

%% -----------------------------------------------------------------------------
%% @doc Наибольший общий делитель двух целых чисел
%% @end
%% -----------------------------------------------------------------------------
-spec gcd(N, M) -> Return when
    N      :: integer(),
    M      :: integer(),
    Return :: pos_integer().

gcd(N, 0) ->
    abs(N);
gcd(N, M) ->
    gcd(M, N rem M).

%% -----------------------------------------------------------------------------
%% @doc Знак числа
%% @end
%% -----------------------------------------------------------------------------
-spec sign(N) -> Return when
    N      :: number(),
    Return :: -1 | 0 | 1.

sign(N) when N == 0 -> 0;
sign(N) when is_number(N) ->
    case N > 0 of
        true  -> 1;
        false -> -1
    end.

%% -----------------------------------------------------------------------------
%% @doc Функция, возвращающая свой аргумент
%% @end
%% -----------------------------------------------------------------------------
-spec id(Term) -> Return when
    Term   :: any(),
    Return :: any().

id(Term) ->
    Term.

%% -----------------------------------------------------------------------------
%% @doc Применить функцию ко всем элементам списка
%% @end
%% -----------------------------------------------------------------------------
-spec map(F, L) -> Return when
    F      :: fun(),
    L      :: [any()],
    Return :: [any()].

map(F, L) ->
    [F(X) || X <- L].

%% -----------------------------------------------------------------------------
%% @doc Фильтрация списка по условию
%% @end
%% -----------------------------------------------------------------------------
-spec filter(P, L) -> Return when
    P      :: fun(),
    L      :: [any()],
    Return :: [any()].

filter(P, L) ->
    [X || X <- L, P(X)].

%% -----------------------------------------------------------------------------
%% @doc Очистка очереди сообщений текущего процесса
%% @end
%% -----------------------------------------------------------------------------
-spec flush() -> Return when
    Return :: ok.

flush() ->
    receive
        _ -> flush()
    after
        0 -> ok
    end.

%% -----------------------------------------------------------------------------
%% @doc Остановка текущего процесса на T миллисекунд
%% @end
%% -----------------------------------------------------------------------------
-spec sleep(T) -> Return when
    T      :: non_neg_integer,
    Return :: ok.

sleep(T) when is_integer(T), T >= 0 ->
    receive
        after T -> ok
    end.

%% -----------------------------------------------------------------------------
%% @doc Обработчик завершения процесса
%% Если процесс Pid умирает с причиной Why, то вычисляется функция F(Why)
%% @end
%% -----------------------------------------------------------------------------
-spec on_exit(Pid, F) -> Return when
    Pid    :: pid(),
    F      :: fun(),
    Return :: any().

on_exit(Pid, F) ->
    spawn(
        fun() ->
            process_flag(trap_exit, true),
            link(Pid),
            receive
                {'EXIT', Pid, Why} -> F(Why)
            end
        end).

%% -----------------------------------------------------------------------------
%% @doc Цикл for от M до N с шагом S
%% @end
%% -----------------------------------------------------------------------------
-spec for(M, N, S, F) -> Return when
    M      :: integer(),
    N      :: integer(),
    S      :: integer(),
    F      :: fun(),
    Return :: any().

for(M, N, S, F) ->
    [F(X) || X <- lists:seq(M, N, S)].

%% -----------------------------------------------------------------------------
%% @doc Цикл for от M до N с шагом 1
%% @end
%% -----------------------------------------------------------------------------
-spec for(M, N, F) -> Return when
    M      :: integer(),
    N      :: integer(),
    F      :: fun(),
    Return :: any().

for(M, N, F) ->
    for(M, N, 1, F).

%% -----------------------------------------------------------------------------
%% @doc Цикл for от 1 до N с шагом 1
%% @end
%% -----------------------------------------------------------------------------
-spec for(N, F) -> Return when
    N      :: integer(),
    F      :: fun(),
    Return :: any().

for(N, F) ->
    for(1, N, 1, F).
