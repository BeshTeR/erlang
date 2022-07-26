%%% ----------------------------------------------------------------------------
%%% @author Oleg Meraviev <avesh.net@bk.ru>
%%%
%%% @doc Частотный анализ текстового файла
%%% @end
%%% ----------------------------------------------------------------------------
-module(stat_text).

%% API
-export([start/1]).

%% Tests
-include("tests/stat_text_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Запуск расчёта стстистики по файлу File
%% @end
%% -----------------------------------------------------------------------------
-spec start(File) -> Return when
    File   :: string(),
    Return :: ok.

start(File) when is_list(File) ->
    case file:open(File, [read]) of
        {ok, IODevice} ->
            ets:new(db, [named_table, ordered_set]),
            lines(IODevice),
            out_result(File),
            stop(IODevice);
        {error, Reason} ->
            io:format("Ошибка при открытии файла: \"~s\": ~w~n", [File, Reason])
    end.

%% Private ---------------------------------------------------------------------

%% завершение работы
stop(IODevice) ->
    case file:close(IODevice) of
        ok -> ok;
        {error, Reason} ->
            io:format("Ошибка при закрытии файла: ~w~n", [Reason])
    end,
    ets:delete(db), ok.

%% читаем очередную строку из файла
lines(IODevice) ->
    case io:get_line(IODevice, "") of
        eof -> ok;
        Line ->
            line(Line),
            lines(IODevice)
    end.

%% символы - разделители слов
-define(Punctuation, " \t\n.,;:-()[]{}|/><*~!@#?$%^&").

%% разбираем строку на слова
line(Line) ->
    Res = string:tokens(Line, ?Punctuation),
    [add(string:to_upper(X)) || X <- Res].

%% добавляем слово в базу
add(X) ->
    case ets:lookup(db,X) of
        [] -> ets:insert(db, {X, 1});
        [{X, N}] -> ets:insert(db, {X, N+1})
    end.

%% выводим результаты расчетов
out_result(File) ->
    io:format("Частоты по словам:~n"),
    {Chars, Words, Count} = out_words(ets:first(db), {0, 0, 0}),
    case Count =/= 0 of
        true ->
            io:format("Статистика по файлу \"~s\":~n", [File]),
            io:format("Всего букв: ~w~n", [Chars]),
            io:format("Различных слов: ~w~n", [Words]),
            io:format("Средняя длина слова: ~.2f~n", [Chars/Count]);
        false ->
            io:format("Файлу \"~s\" пуст~n", [File])
    end.

%% выводим чистоты по словам и считаем статистику
out_words('$end_of_table', Acc) -> Acc;
out_words(Word, {Chars, Words, Count}) ->
    case ets:lookup(db, Word) of
        [{Word, N}] ->
            io:format("~s -> ~w~n", [Word, N]),
            out_words(ets:next(db, Word), {Chars+N*length(Word), Words+1, Count+N});
        [] ->
            io:format("Ошибка: слово \"~s\" в статистике отсутствует~n", [Word])
    end.
