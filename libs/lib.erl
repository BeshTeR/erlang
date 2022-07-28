%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека некоторых часто используемых функций
%%% @end
%%% ----------------------------------------------------------------------------
-module(lib).

%% API
-export([gcd/2, sign/1, pow/2, fac/1, mult/2, id/1, pmap/2, pcall/1, flush/0, on_exit/2, type_of/1, count_msg/1, depth/1, map_all/2]).

%% Tests
-include("tests/lib_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Быстрое возведение числа в степень по инвариантам:
%% N^M = (N^(M/2))^2 - если M четно
%% N^M = N*(N^(M-1)) - если M нечетно
%% При целом N и при M > 0 результат - целое число, в отличии от math:pow/2, где результат = float()
%% @end
%% -----------------------------------------------------------------------------
-spec pow(N, M) -> Return when
    N      :: number(),
    M      :: integer(),
    Return :: number().

pow(N, M) when is_number(N), is_integer(M) ->
    if M >= 0 -> pow(N, M, 1);
        true -> 1/pow(N, -M, 1)
    end.

% тело pow с аккумулятором
pow(_, 0, Res) ->
    Res;
pow(N, M, Res) when M rem 2 =:= 0 ->
    pow(N*N, M div 2, Res);
pow(N, M, Res) ->
    pow(N, M-1, N*Res).

%% -----------------------------------------------------------------------------
%% @doc Быстрое произведение натуральных чисел от M до N
%% @end
%% -----------------------------------------------------------------------------
-spec mult(M, N) -> Result when
    M      :: non_neg_integer(),
    N      :: non_neg_integer(),
    Result :: pos_integer().

mult(M, N) when M > 0, M =< N ->
    X = (M+N) div 2,
    [A, B] = pmap(fun mult/1, [{M, X}, {X+1, N}]),
    A*B.

mult({A, A}) -> A;
mult({A, B}) ->
    X = (A+B) div 2,
    mult({A, X})*mult({X+1, B}).

%% -----------------------------------------------------------------------------
%% @doc Быстрое вычисление факториала натурального числа N
%% @end
%% -----------------------------------------------------------------------------
-spec fac(N) -> Return when
    N      :: non_neg_integer(),
    Return :: pos_integer().

fac(0) -> 1;
fac(1) -> 1;
fac(N) -> mult(1, N).

%% -----------------------------------------------------------------------------
%% @doc Наибольший общий делитель двух целых чисел
%% @end
%% -----------------------------------------------------------------------------
-spec gcd(N, M) -> Return when
    N      :: integer(),
    M      :: integer(),
    Return :: pos_integer().

gcd(N, 0) -> abs(N);
gcd(N, M) -> gcd(M, N rem M).

%% -----------------------------------------------------------------------------
%% @doc Знак числа
%% @end
%% -----------------------------------------------------------------------------
-spec sign(N) -> Return when
    N      :: number(),
    Return :: -1 | 0 | 1.

sign(N) when N == 0 -> 0;       %% матчится 0 и 0.0
sign(N) when is_number(N) ->
    if N > 0 -> 1;
        true -> -1
    end.

%% -----------------------------------------------------------------------------
%% @doc Функция, возвращающая свой аргумент
%% @end
%% -----------------------------------------------------------------------------
-spec id(Term) -> Return when
    Term   :: any(),
    Return :: any().

id(Term) -> Term.

%% -----------------------------------------------------------------------------
%% @doc Применить функцию F параллельно ко всем элементам списка L (параллельный аналог lists:map/2).
%% При "легкой" F быстрее работает lists:map/2, но при "тяжелой" F быстрее работает уже lib:pmap/2.
%% @end
%% -----------------------------------------------------------------------------
-spec pmap(F, L) -> Return when
    F      :: fun(),
    L      :: [any()],
    Return :: [any() | {error, any()}].

pmap(F, L) ->
    process_flag(trap_exit, true),
    Id = make_ref(),
    %% Запускаем процессы вычисления функции
    [spawn_link(
        fun() ->
            receive
                {Id, Pid, F, X} -> Pid ! {Id, self(), F(X)}
            end
        end) ! {Id, self(), F, X} || X <- L],
    %% Собираем результаты вычислений в список
    [X || {_,X} <- lists:sort(
        fun({A,_}, {B,_}) ->
            A < B
        end,
        [receive
             {Id, Pid, Res} -> {Pid, Res};
             {'EXIT', Pid, Reason} when Reason =/= normal -> {Pid, {error, Reason}}
         end || _ <- lists:seq(1, length(L))])].

%% -----------------------------------------------------------------------------
%% @doc Параллельный запуск списка функций
%% @end
%% -----------------------------------------------------------------------------
-spec pcall(L) -> Return when
    L      :: [{atom(), atom(), [any()]}],
    Return :: [any() | {error, any()}].

pcall(L) ->
    process_flag(trap_exit, true),
    Id = make_ref(),
    %% Запускаем процессы вычисления функции
    [spawn_link(
        fun() ->
            receive
                {Id, Pid, M, F, Args} -> Pid ! {Id, self(), apply(M, F, Args)}
            end
        end) ! {Id, self(), M, F, Args} || {M, F, Args} <- L],
    %% Собираем результаты вычислений в список
    [X || {_,X} <- lists:sort(
        fun({A,_}, {B,_}) ->
            A < B
        end,
        [receive
             {Id, Pid, Res} -> {Pid, Res};
             {'EXIT', Pid, Reason} when Reason =/= normal -> {Pid, {error, Reason}}
         end || _ <- lists:seq(1, length(L))])].

%% -----------------------------------------------------------------------------
%% @doc Очистка очереди сообщений текущего процесса
%% @end
%% -----------------------------------------------------------------------------
-spec flush() -> Return when
    Return :: ok.

flush() ->
    receive
        _ -> flush()
        after 0 -> ok
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
%% @doc Количество сообщений в почтовом ящике процесса
%% @end
%% -----------------------------------------------------------------------------
-spec count_msg(Pid) -> Return when
    Pid    :: pid(),
    Return :: pos_integer().

count_msg(Pid) ->
    {message_queue_len, Length} = process_info(Pid, message_queue_len),
    Length.

%% -----------------------------------------------------------------------------
%% @doc Тип аргумента
%% @end
%% -----------------------------------------------------------------------------
-spec type_of(X) -> Return when
    X      :: any(),
    Return :: atom | boolean | integer | real | list | tuple | map | binary | pid | port | function | reference.

type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_atom(X)      -> atom;
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> real;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_map(X)       -> map;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_binary(X)    -> binary.

%% -----------------------------------------------------------------------------
%% @doc Максимальная глубина списка
%% @end
%% -----------------------------------------------------------------------------
-spec depth(X) -> Return when
    X :: any(),
    Return :: non_neg_integer().

depth(X) when not is_list(X) -> 0;
depth([]) -> 1;
depth([H|L]) -> max(depth(H)+1, depth(L)).

%% -----------------------------------------------------------------------------
%% @doc Применить функцию F ко всем элементам L согласно правила:
%% если L - не список, то = F(L),
%% если L - список, то = применить F ко всем элементам L на и ко всем элементам его подсписков
%% @end
%% -----------------------------------------------------------------------------
-spec map_all(F, L) -> Return when
    F      :: fun(),
    L      :: any(),
    Return :: any().

map_all(F, X) when not is_list(X) -> F(X);
map_all(_, []) -> [];
map_all(F, L) -> [map_all(F, X) || X <- L].
