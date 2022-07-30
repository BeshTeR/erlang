%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с цепными дробями
%%% @end
%%% ----------------------------------------------------------------------------
-module(chain).

%% API -------------------------------------------------------------------------

%% Конструкторы и деструкторы
-export([make/2, make/3, split/1]).

%% Предикаты
-export([is_chain/1, is_infinity/1]).

%% Индексирование элементов цепной дроби и ее глубина
-export([depth/1, depth/2, nth/2]).

%% Математические операции
-export([mult/2, division/2, rev/1,  add/2, sub/2]).

%% Логические операции сравнения
-export([equal/2, less/2, greq/2, under/2]).

%% Преобразования
-export([to_string/1, to_list/1, to_rat/1, to_float/1, from_natural/1, from_rat/1, from_float/2]).

%% Некоторые математические величины, выражаемые цепными дробями
-export([fi/0, sqrt2/0, pi/0]).

%% Tests -----------------------------------------------------------------------
-include("tests/chain_tests.erl").

%% Тип - цепная дробь ----------------------------------------------------------
-type chain() :: {pos_integer(),    % целая часть дроби
                 [pos_integer()],   % конечная часть дроби
                 [pos_integer()]}.  % бесконечная (периодическая) часть дроби

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
                    [_,H2|T] = lists:reverse(L1),
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

split({N, L1, L2}) when is_integer(N), N >= 0 ->
    case is_list_integer(L1) andalso is_list_integer(L2) of
        true -> {N, L1, L2};
        false -> {error, bad_format}
    end;
split(_) -> {error, bad_format}.

%% это список целых положительных чисел ?
is_list_integer([]) -> true;
is_list_integer([H|T]) when is_integer(H), H > 0 -> is_list_integer(T);
is_list_integer(_) -> false.

%% -----------------------------------------------------------------------------
%% @doc Это цепная дробь ?
%% @end
%% -----------------------------------------------------------------------------
-spec is_chain(C) -> Return when
    C      :: any(),
    Return :: boolean().

is_chain(C) ->
    case split(C) of
        {_, _, _} -> true;
        {error, bad_format} -> false
    end.

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
%% @doc Преобразовать конечную цепную дробь в список
%% @end
%% -----------------------------------------------------------------------------
-spec to_list(C) -> Return when
    C      :: chain(),
    Return :: [pos_integer()] | undefined.

to_list(C) ->
    {N, L1, L2} = split(C),
    case L2 =/= [] of
        true -> undefined;
        false -> [N|L1]
    end.

%% -----------------------------------------------------------------------------
%% @doc Преобразовать конечную цепную дробь в рациональной число
%% @end
%% -----------------------------------------------------------------------------
-spec to_rat(C) -> Return when
    C      :: chain(),
    Return :: rat:rat() | undefined.

to_rat(C) ->
    {N, L1, L2} = split(C),
    case L2 =:= [] of
        true ->
            [H|T] = lists:reverse([N|L1]),
            {Num, Den} = to_rat(T, {H, 1}),
            rat:make(Num, Den);
        false -> undefined
    end.

to_rat([], Acc) -> Acc;
to_rat([H|T], {Num, Den}) -> to_rat(T, {Num*H+Den, Num}).

%% -----------------------------------------------------------------------------
%% @doc Преобразовать конечную цепную дробь в вещественное число
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

from_rat(R) ->
    M = rat:numerator(R),
    N = rat:denominator(R),
    [H|T] = list_nums(M, N, []),
    make(H, T).

list_nums(M, 1, Acc) -> lists:reverse([M|Acc]);
list_nums(M, N, Acc) ->
    Z = M div N,
    list_nums(N, M-N*Z, [Z|Acc]).

%% -----------------------------------------------------------------------------
%% @doc Преобразовать вещественное число в цепную дробь (учитывая N знаков после запятой)
%% @end
%% -----------------------------------------------------------------------------
-spec from_float(X, N) -> Return when
    X      :: float(),
    N      :: non_neg_integer(),
    Return :: chain().

from_float(X, N) when is_float(X), is_integer(N), N >= 0 -> from_rat(rat:from_float(X, N)).

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

%% -----------------------------------------------------------------------------
%% @doc Сравнение цепных дробей на равенство (C1 =:= C2)
%% @end
%% -----------------------------------------------------------------------------
-spec equal(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: boolean().

equal(C1, C2) ->
    N = max(min_len(C1), min_len(C2)),
    in_list(depth(N, C1)) =:= in_list(depth(N, C2)).

%% минимальное количество различных чисел в цепной дроби
min_len(C) ->
    {_, L1, L2} = split(C),
    length(L1)+length(L2)+1.

%% собрать элементы цепной дроби в список
in_list(C) ->
    {N, L1, L2} = split(C),
    [N|L1] ++ L2.

%% -----------------------------------------------------------------------------
%% @doc Сравнение цепных дробей на больше (C1 > C2)
%% @end
%% -----------------------------------------------------------------------------
-spec greq(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: boolean().

greq(C1, C2) -> compare(C1, C2, true).

%% -----------------------------------------------------------------------------
%% @doc Сравнение цепных дробей на меньше (C1 < C2)
%% @end
%% -----------------------------------------------------------------------------
-spec less(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: boolean().

less(C1, C2) -> compare(C1, C2, false).

%% сравнить поэлементно
compare([], [], _) -> false;
compare([], _, F) -> not F;
compare(_, [], F) -> F;
compare([H|T1], [H|T2], F) -> compare(T1, T2, not F);
compare([H1|_], [H2|_], F) when H1 > H2 -> F;
compare([H1|_], [H2|_], F) when H1 < H2 -> not F;
compare(C1, C2, F) ->
    N = max(min_len(C1), min_len(C2)),
    [H1|T1] = L1 = in_list(depth(N, C1)),
    [H2|T2] = L2 = in_list(depth(N, C2)),
    case H1 =:= H2 andalso ((T1 =:= []) xor (T2 =:= [])) of
        true ->
            case F of
                true -> T1 =/= [];
                false -> T2 =/= []
            end;
        false -> compare(L1, L2, F)
    end.

%% -----------------------------------------------------------------------------
%% @doc Цепная дробь является поддробью другой цепной дроби (C1 <- C2)
%% @end
%% -----------------------------------------------------------------------------
-spec under(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: boolean().

under(C1, C2) ->
    N = max(min_len(C1), min_len(C2)),
    S1 = in_list(depth(N, C1)),
    S2 = in_list(depth(N, C2)),
    lists:sublist(S2, length(S1)) =:= S1.

%% -----------------------------------------------------------------------------
%% @doc Умножение конечных цепных дробей (C1 * C2)
%% @end
%% -----------------------------------------------------------------------------
-spec mult(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: chain().

mult(C1, C2) -> from_rat(rat:mult(to_rat(C1),to_rat(C2))).

%% -----------------------------------------------------------------------------
%% @doc Деление конечных цепных дробей (C1 / C2)
%% @end
%% -----------------------------------------------------------------------------
-spec division(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: chain().

division(C1, C2) -> from_rat(rat:division(to_rat(C1),to_rat(C2))).

%% -----------------------------------------------------------------------------
%% @doc Сложение конечных цепных дробей (C1 + C2)
%% @end
%% -----------------------------------------------------------------------------
-spec add(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: chain().

add(C1, C2) -> from_rat(rat:add(to_rat(C1),to_rat(C2))).

%% -----------------------------------------------------------------------------
%% @doc Модуль разности конечных цепных дробей (C1 - C2)
%% @end
%% -----------------------------------------------------------------------------
-spec sub(C1, C2) -> Return when
    C1     :: chain(),
    C2     :: chain(),
    Return :: chain().

sub(C1, C2) ->
    case greq(C1, C2) of
        true -> from_rat(rat:sub(to_rat(C1),to_rat(C2)));
        false -> from_rat(rat:sub(to_rat(C2),to_rat(C1)))
    end.

%% -----------------------------------------------------------------------------
%% @doc Цепная дробь обратная данной (1/C)
%% @end
%% -----------------------------------------------------------------------------
-spec rev(C) -> Return when
    C      :: chain(),
    Return :: chain() | {error, division_by_zero}.

rev(C) ->
    {N, L1, L2} = split(C),
    case N =:= 0 of
        true ->
            case L1 =:= [] andalso L2 =:= [] of
                true -> {error, division_by_zero};
                false ->
                    [H|T] =
                    case L1 =/= [] of
                        true -> L1;
                        false -> L2
                    end,
                    make(H, T, L2)
            end;
        false -> make(0, [N|L1], L2)
    end.

%% Некоторые математические величины, выражаемые цепными дробями:

%% "золотое сечение", число fi (точно)
fi() -> chain:make(1,[],[1]).

%% квадратный корень из 2 (точно)
sqrt2() -> chain:make(1,[],[2]).

%% число pi (приближение конечной цепной дробью)
pi() -> chain:make(3,[7,15,1,292,1,1,1,2,1,3,1,14,2,1,1,2,2,2,2,1,84,2,1,1,15]).
