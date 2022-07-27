%%% ----------------------------------------------------------------------------
%%% @author Oleg Meraviev <avesh.net@bk.ru>
%%%
%%% @doc Частотный анализ текстового файла
%%% @end
%%% ----------------------------------------------------------------------------
-module(stat_text).

%% API
-export([make/1, out/1, write/1, convert/1]).

%% Tests
-include("tests/stat_text_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Запуск расчёта статистики по текстовому файлу
%% @end
%% -----------------------------------------------------------------------------
-spec make(File) -> Return when
    File   :: string(),
    Return :: ok.

make(File) when is_list(File) ->
    In = file(File, txt),
    Out = file(File, base),
    case file:open(In, [read]) of
        {ok, IODevice} ->
            init(),
            lines(IODevice),
            save(Out),
            out_result(In),
            stop();
        {error, Reason} ->
            io:format("Ошибка при открытии файла: \"~s\": ~w~n", [In, Reason])
    end.

%% -----------------------------------------------------------------------------
%% @doc Преобразование частотной базы в текстовый формат
%% @end
%% -----------------------------------------------------------------------------
-spec convert(File) -> Return when
    File   :: string(),
    Return :: ok.

convert(File) ->
    case load(file(File, base)) of
        ok ->
            base_to_stat(File),
            stop();
        {error, Reason} -> {error, Reason}
    end.

%% -----------------------------------------------------------------------------
%% @doc Вывод статистики по частотной базе из файла
%% @end
%% -----------------------------------------------------------------------------
-spec out(File) -> Return when
    File   :: string(),
    Return :: ok | {error, any()}.

out(File) ->
    FileName = file(File, base),
    case load(FileName) of
        ok ->
            out_result(FileName),
            stop();
        {error, Reason} -> {error, Reason}
    end.

%% -----------------------------------------------------------------------------
%% @doc Вывод на экран частотной базы из файла
%% @end
%% -----------------------------------------------------------------------------
-spec write(File) -> Return when
    File   :: string(),
    Return :: ok.

write(File) ->
    FileName = file(File, base),
    case load(FileName) of
        ok ->
            write_base(standard_io, ets:first(db)),
            stop();
        {error, Reason} -> {error, Reason}
    end.

%% Private ---------------------------------------------------------------------

%% инициация базы
init() ->
    case ets:info(db) of
        undefined -> ets:new(db, [named_table, ordered_set]);
        _ -> ets:delete_all_objects(db)
    end.

%% закрываем базу
stop() ->
    case ets:info(db) of
        undefined -> ok;
        _ -> ets:delete(db), ok
    end.

%% завершение работы с файлом
stop(standard_io) -> ok;
stop(IODevice) ->
    case file:close(IODevice) of
        ok -> ok;
        {error, Reason} ->
            io:format("Ошибка при закрытии файла: ~w~n", [Reason]),
            {error, Reason}
    end.

%% читаем очередную строку из файла
lines(IODevice) ->
    case io:get_line(IODevice, "") of
        eof ->
            stop(IODevice);
        Line ->
            line(Line),
            lines(IODevice)
    end.

%% символы - разделители слов
-define(Punctuation, "_ \t\n.,;:-()[]{}|/><*~!@#?$%^&`\"").

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
    {Chars, Words, Count} = stat(ets:first(db), {0, 0, 0}),
    case Count =/= 0 of
        true ->
            io:format("Статистика (\"~s\"):~n", [File]),
            io:format("Всего букв: ~w~n", [Chars]),
            io:format("Различных слов: ~w~n", [Words]),
            io:format("Средняя длина слова: ~.2f~n", [Chars/Count]);
        false ->
            io:format("Файл \"~s\" пуст~n", [File])
    end.

%% считаем статистику
stat('$end_of_table', Acc) -> Acc;
stat(Word, {Chars, Words, Count}) ->
    case ets:lookup(db, Word) of
        [{Word, N}] ->
            stat(ets:next(db, Word), {Chars+N*length(Word), Words+1, Count+N});
        [] ->
            io:format("Ошибка: слово \"~s\" в статистике отсутствует~n", [Word])
    end.

%% загружаем в текущую частотную базу данные из файла
load(File) ->
    stop(),
    case ets:file2tab(File) of
        {ok, _} -> ok;
        {error, Reason} ->
            io:format("Ошибка при открытии файла: \"~s\": ~w~n", [File, Reason]),
            {error, Reason}
    end.

%% записываем текущую частотную базу в файл
save(File) ->
    case ets:tab2file(db, File) of
        ok -> ok;
        {error, Reason} ->
            io:format("Ошибка при записи в файл: \"~s\": ~w~n", [File, Reason]),
            {error, Reason}
    end.

%% сохраняем текущую базу как текстовый файл
base_to_stat(File) ->
    FileName = file(File, stat),
    case file:open(FileName, [write]) of
        {ok, IODevice} ->
            write_base(IODevice, ets:first(db)),
            stop();
        {error, Reason} ->
            io:format("Ошибка при открытии файла: \"~s\": ~w~n", [FileName, Reason])
    end.

%% выводв текстовый файл (на экран) текущей частотной базы
write_base(IODevice, '$end_of_table') -> stop(IODevice);
write_base(IODevice, Word) ->
    case ets:lookup(db, Word) of
        [{Word, N}] ->
            io:fwrite(IODevice, "~s = ~w~n", [Word, N]),
            write_base(IODevice, ets:next(db, Word));
        [] ->
            io:format("Ошибка: слово \"~s\" в статистике отсутствует~n", [Word])
    end.

%% добавляем расширение к имени файла
file(File, txt) -> File ++ ".txt";
file(File, base) -> File ++ ".base";
file(File, stat)  -> File ++ ".stat";
file(_, X) ->
    io:format("Ошибка: недопустимый параметр типа файла: ~w~n", [atom_to_list(X)]).
