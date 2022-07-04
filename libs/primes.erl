%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с простыми числами
%%% @end
%%% ----------------------------------------------------------------------------
-module(primes).

%% API
-export([is/1, list/1, seq/2, factors/1, rnd/2, less/1, next/1, twins/1, count/1]).

%% Tests
-include("tests/primes_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Проверка числа на простоту
%% @end
%% -----------------------------------------------------------------------------
-spec is(N) -> Return when
    N      :: integer(),
    Return :: boolean().

is(N) when is_integer(N), N < 0 ->
    is(-N);
is(N) when not is_integer(N); N < 2 ->
    false;
is(N) when N rem 2 == 0 ->
    N == 2;
is(N) ->
    is(N, 3).

%% тело is с аккумулятором
is(N, Acc) when Acc * Acc > N ->
    true;
is(N, Acc) when N rem Acc == 0 ->
    false;
is(N, Acc) ->
    is(N, Acc+2).

%% -----------------------------------------------------------------------------
%% @doc Наибольшее простое число, не большее заданного натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec less(N) -> Result when
    N      :: pos_integer(),
    Result :: no_primes | pos_integer().

less(1) ->
    no_primes;
less(2) ->
    2;
less(N) when N rem 2 == 0 ->
    less(N-1);
less(N) when is_integer(N), N > 1 ->
    case is(N) of
        true  -> N;
        false -> less(N-2)
    end.

%% -----------------------------------------------------------------------------
%% @doc Наименьшее простое число, не меньшее заданного натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec next(N) -> Result when
    N      :: non_neg_integer(),
    Result :: pos_integer().

next(N) when is_integer(N), N >= 0, N < 3 ->
    2;
next(N) when N rem 2 == 0 ->
    next(N+1);
next(N) when is_integer(N) ->
    case is(N) of
        true  -> N;
        false -> next(N+2)
    end.

%% -----------------------------------------------------------------------------
%% @doc Список простых чисел меньших или равных заданного натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec list(N) -> Return when
    N      :: pos_integer(),
    Return :: [integer()].

list(1) ->
    [];
list(N) when is_integer(N), N > 1 ->
    Set = ordsets:from_list(lists:seq(3, N, 2)),
    ordsets:add_element(2, sieve(N, Set, Set, ordsets:new())).

sieve(_, Set1, Set2, Primes) when length(Set2) == 0 ->
    ordsets:union(Primes, Set1);
sieve(N, Set1, Set2, Primes) ->
    H = lists:nth(1,string:substr(Set1, 1, 1)),
    {R1, R2} = remove_multiples_of(H, ordsets:del_element(H, Set1), Set2),
    sieve(N, R1, R2, ordsets:add_element(H, Primes)).

remove_multiples_of(N, Set1, Set2) ->
    NewSet = ordsets:filter(fun(X) -> X >= N*N end, Set2),
    R = ordsets:filter(fun(X) -> X rem N == 0 end, NewSet),
    {ordsets:subtract(Set1, R), ordsets:subtract(NewSet, R)}.

%% -----------------------------------------------------------------------------
%% @doc Простые числа-близнецы, не превышающие заданного натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec twins(N) -> Return when
    N      :: pos_integer(),
    Return :: no_primes | {pos_integer(), pos_integer()}.

twins(N) when is_integer(N) ->
    X = less(N),
    case X == no_primes orelse X < 5 of
        true -> no_primes;
        false ->
            case is(X-2) of
                true  -> {X-2, X};
                false -> twins(X-4)
            end
    end.

%% -----------------------------------------------------------------------------
%% @doc Список простых чисел из заданного диапазона значений
%% @end
%% -----------------------------------------------------------------------------
-spec seq(M, N) -> Return when
    M      :: pos_integer(),
    N      :: pos_integer(),
    Return :: [integer()].

seq(M, N) when is_integer(M), is_integer(M), M > 0, N > 0 ->
    [X || X <- list(N), X >= M].

%% -----------------------------------------------------------------------------
%% @doc Список простых множителей натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec factors(N) -> Return when
    N      :: pos_integer(),
    Return :: [pos_integer()].

factors(1) ->
    [];
factors(N) when is_integer(N), N > 1 ->
    Res = factors(N, {2, []}),
    case Res of
        [_|_] -> lists:reverse(Res);
        _       -> []
    end.

%% тело factors с аккумулятором
factors(N, {M, Acc}) when M * M > N ->
    [N | Acc];
factors(N, {M, Acc}) when N rem M == 0 ->
    factors(N div M, {M, [M | Acc]});
factors(N, {M, Acc}) ->
    factors(N, {M+1, Acc}).

%% -----------------------------------------------------------------------------
%% @doc Количество простых чисел меньших или равных натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec count(N) -> Return when
    N      :: pos_integer(),
    Return :: pos_integer().

count(N) when is_integer(N), N > 0 ->
    length(list(N)).

%% -----------------------------------------------------------------------------
%% @doc Случайное простое число из заданного диапазона
%% @end
%% -----------------------------------------------------------------------------
-spec rnd(M, N) -> Return when
    M      :: pos_integer(),
    N      :: pos_integer(),
    Return :: no_primes | pos_integer().

rnd(M, N) when is_integer(M), is_integer(N), M > 0, N > 0 ->
    case ListPrimes = seq(M, N) of
        []  -> no_primes;
        _   -> lists:nth(rnd:new(1, length(ListPrimes)), ListPrimes)
    end.
