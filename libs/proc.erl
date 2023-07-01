%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека функций работы с процессами
%%% @end
%%% ----------------------------------------------------------------------------
-module(proc).

%% API -------------------------------------------------------------------------
-export([flush/0, on_exit/2, count_msg/1]).

%% Tests -----------------------------------------------------------------------
-include("tests/proc_tests.erl").

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
