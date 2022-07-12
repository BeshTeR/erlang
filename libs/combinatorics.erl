%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека функций комбинаторики
%%% @end
%%% ----------------------------------------------------------------------------
-module(combinatorics).

%% API
-export([placements/2, placements_rep/2, permutations/1, combinations/2, combinations_rep/2]).
-export([bin/2, perms/1]).

%% Tests
-include("tests/combinatorics_tests.erl").

%% Тип: натуральое число
-type natural() :: non_neg_integer().

%% -----------------------------------------------------------------------------
%% @doc Размещения из N по K
%% Упорядоченный набор из K различных элементов некоторого множества различных N элементов.
%% @end
%% -----------------------------------------------------------------------------
-spec placements(N, K) -> Result when
    N      :: natural(),
    K      :: natural(),
    Result :: natural().

placements(N, K) when is_integer(N), is_integer(K), K >= 0, N >= K ->
    lib:fac(N) div (lib:fac(N-K)).

%% -----------------------------------------------------------------------------
%% @doc Размещения с повторениями из N по K
%% Размещение с повторениями или выборка с возвращением — это размещение «предметов» в предположении,
%% что каждый «предмет» может участвовать в размещении несколько раз.
%% @end
%% -----------------------------------------------------------------------------
-spec placements_rep(N, K) -> Result when
    N      :: natural(),
    K      :: natural(),
    Result :: natural().

placements_rep(N, K) when is_integer(N), is_integer(K), K >= 0, N >= K ->
    lib:pow(N, K).

%% -----------------------------------------------------------------------------
%% @doc Перестановки из N различных элементов
%% @end
%% -----------------------------------------------------------------------------
-spec permutations(N) -> Result when
    N      :: natural(),
    Result :: natural().

permutations(N) when is_integer(N), N >= 0 ->
    lib:fac(N).


%% -----------------------------------------------------------------------------
%% @doc Сочетания из N по K
%% Набор из K элементов, выбранных из N N-элементного множества, в котором не учитывается порядок элементов.
%% @end
%% -----------------------------------------------------------------------------
-spec combinations(N, K) -> Result when
    N      :: natural(),
    K      :: natural(),
    Result :: natural().

combinations(N, K) when is_integer(N), is_integer(K), K >= 0, N >= K ->
    bin(N, K).

%% -----------------------------------------------------------------------------
%% @doc Сочетания с повторениями из N по K
%% K-элементный набор из N N-элементного множества, в котором каждый элемент может участвовать несколько раз,
%% но в котором порядок не учитывается (мультимножество).
%% @end
%% -----------------------------------------------------------------------------
-spec combinations_rep(N, K) -> Result when
    N      :: natural(),
    K      :: natural(),
    Result :: natural().

combinations_rep(N, K) when is_integer(N), is_integer(K), N > 0, N >= K ->
    lib:fac(N+K-1) div (lib:fac(K)*lib:fac(N-1)).

%% -----------------------------------------------------------------------------
%% @doc Биномиальные коэффициенты
%% @end
%% -----------------------------------------------------------------------------
-spec bin(N, K) -> Return when
    N      :: natural(),
    K      :: natural(),
    Return :: natural().

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
%% @doc Список всех перестановок из элементов исходного списка
%% @end
%% -----------------------------------------------------------------------------
-spec perms(L) -> Return when
    L      :: [any()],
    Return :: [[any()]].

perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L, T <- perms(L--[H])].
