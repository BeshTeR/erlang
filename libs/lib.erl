%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека некоторых часто используемых функций
%%% @end
%%% ----------------------------------------------------------------------------
-module(lib).

%% API
-export([gcd/2, sign/1, pow/2, fac/1, fib/1, bin/2, id/1, flush/0]).

%% Tests
-include("tests/lib_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Вычисление целой степени числа
%% @end
%% -----------------------------------------------------------------------------
-spec pow(Base, Degree) -> Return when
    Base   :: number(),
    Degree :: integer(),
    Return :: number().

pow(Base, Degree) when is_number(Base), is_integer(Degree) ->
    case Degree >= 0 of
        true  -> pow(Base, Degree, 1);
        false -> 1/pow(Base, -Degree, 1)
    end.

% тело pow с аккумулятором
pow(_, 0, Acc) ->
    Acc;
pow(N, M, Acc) when M rem 2 == 0 ->
    pow(N*N, M div 2, Acc);
pow(N, M, Acc) ->
    pow(N, M-1, N*Acc).

%% -----------------------------------------------------------------------------
%% @doc Числа Фибоначчи
%% @end
%% -----------------------------------------------------------------------------
-spec fib(Num) -> Return when
    Num    :: non_neg_integer(),
    Return :: pos_integer().

fib(Num) when is_integer(Num), Num >= 0 ->
    fib(Num, {1, 0, 0, 1}).

%% тело fib с аккумулятором
fib(0, {_, B, _, _}) ->
    B;
fib(N, {A, B, P, Q}) when N rem 2 == 0 ->
    fib(N div 2, {A, B, P*P+Q*Q, 2*P*Q+Q*Q});
fib(N, {A, B, P, Q}) ->
    fib(N-1, {B*Q+A*Q+A*P, B*P+A*Q, P, Q}).

%% -----------------------------------------------------------------------------
%% @doc Факториал числа
%% @end
%% -----------------------------------------------------------------------------
-spec fac(Num) -> Return when
    Num    :: non_neg_integer(),
    Return :: pos_integer().

fac(Num) when is_integer(Num), Num >=0 ->
    fac(Num, 1).

%% тело fac с аккумулятором
fac(0, Acc) ->
    Acc;
fac(N, Acc) ->
    fac(N-1, N*Acc).

%% -----------------------------------------------------------------------------
%% @doc Биномиальные коэффициенты
%% @end
%% -----------------------------------------------------------------------------
-spec bin(N, K) -> Return when
    N      :: non_neg_integer(),
    K      :: non_neg_integer(),
    Return :: non_neg_integer().

bin(N, K) when is_integer(N), is_integer(K), N >= 0, K >= 0 ->
    bin(N, K, {1, 1}).

%% тело binomial с аккумулятором
bin(N, K, {_, _}) when K > N ->
    0;
bin(_, 0, {P1, P2}) ->
    P1 div P2;
bin(N, K, {P1, P2}) ->
    bin(N-1, K-1, {P1*N, P2*K}).

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
-spec sign(Num) -> Return when
    Num    :: number(),
    Return :: -1 | 0 | 1.

sign(Num) when Num == 0 -> 0;
sign(Num) when is_number(Num) ->
    case Num > 0 of
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
