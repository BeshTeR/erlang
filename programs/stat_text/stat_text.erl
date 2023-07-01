%%% ----------------------------------------------------------------------------
%%% @doc Частотный анализ текстового файла
%%% @end
%%% ----------------------------------------------------------------------------
-module(stat_text).

%% API
-export([make/1, write/1, convert/1]).

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
    FileName = file(File, txt),
    case file:open(FileName, [read]) of
        {ok, IODevice} ->
            init(),
            lines(IODevice),
            save(file(File, base)),
            write_stat(standard_io),
            stop();
        {error, Reason} ->
            io:format("Error opening the file: \"~s\": ~w~n", [FileName, Reason])
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
            FileName = file(File, stat),
            case file:open(FileName, [write]) of
                {ok, IODevice} ->
                    write_stat(IODevice);
                {error, Reason} ->
                    io:format("Error opening the file: \"~s\": ~w~n", [FileName, Reason])
            end,
            stop();
        {error, Reason} -> {error, Reason}
    end.

%% -----------------------------------------------------------------------------
%% @doc Вывод на экран статистики и частотной базы из файла
%% @end
%% -----------------------------------------------------------------------------
-spec write(File) -> Return when
    File   :: string(),
    Return :: ok.

write(File) ->
    case load(file(File, base)) of
        ok ->
            write_stat(standard_io),
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
close(standard_io) -> ok;
close(IODevice) ->
    case file:close(IODevice) of
        ok -> ok;
        {error, Reason} ->
            io:format("Error when closing the file: ~w~n", [Reason]),
            {error, Reason}
    end.

%% читаем очередную строку из файла
lines(IODevice) ->
    case io:get_line(IODevice, "") of
        eof ->
            close(IODevice);
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

%% загружаем в текущую частотную базу данные из файла
load(File) ->
    stop(),
    case ets:file2tab(File) of
        {ok, _} -> ok;
        {error, Reason} ->
            io:format("Error opening the file: \"~s\": ~w~n", [File, Reason]),
            {error, Reason}
    end.

%% записываем текущую частотную базу в файл
save(File) ->
    case ets:tab2file(db, File) of
        ok -> ok;
        {error, Reason} ->
            io:format("Error when writing to a file: \"~s\": ~w~n", [File, Reason]),
            {error, Reason}
    end.

%% вывод в текстовый файл (на экран) текущей частотной базы и статистики
write_stat(IODevice) ->
    write_stat(IODevice, ets:first(db), {0,0,0}).

write_stat(IODevice, '$end_of_table', {Chars, Words, Count}) ->
    case Count =/= 0 of
        true ->
           io:fwrite(IODevice, "Statistics:~n", []),
           io:fwrite(IODevice, "Total letters: ~w~n", [Chars]),
           io:fwrite(IODevice, "Number of different words: ~w~n", [Words]),
           io:fwrite(IODevice, "Average word length: ~.2f~n", [Chars/Count]);
        false ->
            io:format("Error: the current database is empty~n")
    end,
    close(IODevice);

write_stat(IODevice, Word, {Chars, Words, Count}) ->
    case ets:lookup(db, Word) of
        [{Word, N}] ->
            io:fwrite(IODevice, "~s = ~w~n", [Word, N]),
            write_stat(IODevice, ets:next(db, Word), {Chars+N*length(Word), Words+1, Count+N});
        _ ->
            io:format("Error: the word \"~s\" is missing in the database~n", [Word])
    end.

%% добавляем расширение к имени файла
file(File, txt) -> File ++ ".txt";
file(File, base) -> File ++ ".base";
file(File, stat)  -> File ++ ".stat";
file(_, X) ->
    io:format("Error: Invalid file type parameter: ~w~n", [atom_to_list(X)]).
