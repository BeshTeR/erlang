%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека некоторых макроопределений
%%% @end
%%% ----------------------------------------------------------------------------

%% Блокировать вычисление выражения X
-define(QUOTE(X), fun() -> X end).

%% Вычислить блокорованное выражение
-define(EVAL(X), (X)()).
