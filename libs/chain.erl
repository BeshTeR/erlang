%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с цепными дробями (... В РАЗРАБОТКЕ)
%%% @end
%%% ----------------------------------------------------------------------------
-module(chain).

%% API -------------------------------------------------------------------------

%% Конструкторы и деструкторы
-export([make/2, make/3, split/1]).

%% Предикаты
-export([is_chain/1, is_infinity/1]).

%% Индексирование элементов цепной дроби
-export([depth/1, depth/2, nth/2]).

%% Математические операции
-export([]). % .......

%% Логические операции сравнения
-export([]). % .......

%% Преобразования
-export([to_string/1, to_rat/1, to_float/1, from_natural/1, from_rat/1, from_float/1]).

%% Tests -----------------------------------------------------------------------
-include("tests/chain_tests.erl").

%% Тип - цепная дробь ----------------------------------------------------------
-type chain() :: {pos_integer(),    % целая часть дроби
                 [pos_integer()],   % непереодическая часть дроби
                 [pos_integer()]}.  % периодическая часть дроби

%% -----------------------------------------------------------------------------
%% @doc Конструктор конечной цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec make(N, L1) -> Return when
    N      :: pos_integer(),
    L1     :: [pos_integer()],
    Return :: chain() | {error, bad_format}.

make(N, L1) -> make(N, L1, []).

%% -----------------------------------------------------------------------------
%% @doc Конструктор бесконечной цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec make(N, L1, L2) -> Return when
    N      :: non_neg_integer(),
    L1     :: [pos_integer()],
    L2     :: [pos_integer()],
    Return :: chain() | {error, bad_format}.

make(N, [], []) when is_integer(N), N >= 0 -> {N, [], []};
make(N, [1], []) -> make(N+1, [], []);
make(N, L1, []) ->
    C = {N, L1, []},
    case is_chain(C) of
        true ->
            case lists:last(L1) =:= 1 of
                true ->
                    [H1,H2|T] = lists:reverse(L1),
                    {N, lists:reverse([H2+1|T]), []};
                false -> C
            end;
        false -> {error, bad_format}
    end;
make(N, L1, L2) ->
    C = {N, L1, L2},
    case is_chain(C) of
        true -> C;
        false -> {error, bad_format}
    end.

%% -----------------------------------------------------------------------------
%% @doc Разобрать цепную дробь на целую, конечную и бесконечную части
%% @end
%% -----------------------------------------------------------------------------
-spec split(C) -> Return when
    C      :: chain(),
    Return :: {non_neg_integer(), [pos_integer()], [pos_integer()]} | {error, bad_format}.

split(C = {N, L1, L2}) ->
    case is_chain(C) of
        true -> C;
        false -> {error, bad_format}
    end;
split(_) -> {error, bad_format}.

%% -----------------------------------------------------------------------------
%% @doc Это цепная дробь ?
%% @end
%% -----------------------------------------------------------------------------
-spec is_chain(C) -> Return when
    C      :: any(),
    Return :: boolean().

is_chain({N, L1, L2}) when is_integer(N), N >= 0 ->
    is_list_integer(L1) andalso is_list_integer(L2);
is_chain(_) -> false.

%% это список целых положительных чисел ?
is_list_integer([]) -> true;
is_list_integer([H|T]) when is_integer(H), H > 0 -> is_list_integer(T);
is_list_integer(_) -> false.

%% -----------------------------------------------------------------------------
%% @doc Это бесконечная цепная дробь ?
%% @end
%% -----------------------------------------------------------------------------
-spec is_infinity(C) -> Return when
    C      :: any(),
    Return :: boolean().

is_infinity(C) ->
    is_chain(C) andalso
    begin
        {_, _, L2} = split(C),
        L2 =/= []
    end.

%% -----------------------------------------------------------------------------
%% @doc Преобразовать цепную дробь в строку
%% @end
%% -----------------------------------------------------------------------------
-spec to_string(C) -> Return when
    C      :: chain(),
    Return :: string().

to_string(C) ->
    {N, L1, L2} = split(C),
    integer_to_list(N) ++
    case L1 =:= [] of
        true -> "";
        false ->"+" ++ to_string_list(L1)
    end ++
    case L2 =:= [] of
        true -> "";
        false -> "+{" ++ to_string_list(L2) ++ "}"
    end.

to_string_list(L) -> to_string_list(L, "").

to_string_list([], Str) -> Str;
to_string_list([H], Str) ->
    Str ++ "1/" ++ integer_to_list(H);
to_string_list([H|L], Str) ->
    to_string_list(L, Str ++ "1/" ++ integer_to_list(H) ++ "+").

%% -----------------------------------------------------------------------------
%% @doc Преобразовать конечную цепную дробь в рациональной число
%% @end
%% -----------------------------------------------------------------------------
-spec to_rat(C) -> Return when
    C      :: chain(),
    Return :: rat:rat() | undefined.

to_rat({N, L1, []}) -> ok; % ........
to_rat({_, _, _}) -> undefined.

%% -----------------------------------------------------------------------------
%% @doc Преобразовать цепную дробь в вещественное число
%% @end
%% -----------------------------------------------------------------------------
-spec to_float(C) -> Return when
    C      :: chain(),
    Return :: float().

to_float(C) -> rat:to_float(to_rat(C)).

%% -----------------------------------------------------------------------------
%% @doc Преобразовать натуральное число в цепную дробь
%% @end
%% -----------------------------------------------------------------------------
-spec from_natural(N) -> Return when
    N      :: non_neg_integer(),
    Return :: chain() | {error, bad_format}.

from_natural(N) -> make(N, []).

%% -----------------------------------------------------------------------------
%% @doc Преобразовать рациональное число в цепную дробь
%% @end
%% -----------------------------------------------------------------------------
-spec from_rat(R) -> Return when
    R      :: rat:rat(),
    Return :: chain().

from_rat(C) -> ok. % ........

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

depth(C) ->
    {_, L1, L2} = split(C),
    case L2 =:= [] of
        true -> length(L1);
        false -> infinity
    end.

%% -----------------------------------------------------------------------------
%% @doc Установить глубину цепной дроби не более, чем N
%% @end
%% -----------------------------------------------------------------------------
-spec depth(N, C) -> Return when
    N      :: non_neg_integer(),
    C      :: chain(),
    Return :: chain().

depth(N, C) when is_integer(N), N >= 0 ->
    Size = depth(C),
    case Size =/= infinity andalso Size =< N  of
        true -> C;      % усекать нечего
        false -> make(nth(0, C), [nth(I, C) || I <- lists:seq(1,N)])
    end.

%% -----------------------------------------------------------------------------
%% @doc N-й элемент цепной дроби
%% @end
%% -----------------------------------------------------------------------------
-spec nth(N, C) -> Return when
    N      :: non_neg_integer(),
    C      :: chain(),
    Return :: pos_integer() | undefined.

nth(0, C) ->
    {M, _, _} = split(C),
    M;
nth(N, C) when is_integer(N), N > 0 ->
    {_, L1, L2} = split(C),
    case N =< length(L1) of
        true -> lists:nth(N, L1);
        false -> nth_circle(N-length(L1), L2)
    end.

nth_circle(_, []) -> undefined;
nth_circle(N, L) when N =< length(L) -> lists:nth(N, L);
nth_circle(N, L) -> nth_circle(N-length(L), L).
