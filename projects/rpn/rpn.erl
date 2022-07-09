%%% ----------------------------------------------------------------------------
%%% @author Oleg Meraviev <avesh.net@bk.ru>
%%%
%%% @doc Вычислитель выражений в обратной польской записи
%%% @end
%%% ----------------------------------------------------------------------------
-module(rpn).

%% API
-export([eval/1]).

%% Tests
-include("tests/rpn_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Вычислить выражение, заданное строкой в обратной польской записи
%% @end
%% -----------------------------------------------------------------------------
-spec eval(Str) -> Return when
    Str    :: string(),
    Return :: number().

eval(Str) when is_list(Str) ->
    [Res] = lists:foldl(fun eval/2, [], string:tokens(Str, "\n\t ")),
    Res.

%% вычислить выражение в стеке
eval("pi", Stack)          -> [math:pi()|Stack];
eval("!", [N|Stack])       -> [lib:fac(N)|Stack];
eval("sum", Stack)         -> [lists:foldl(fun (X,Y) -> X+Y end, 0, Stack)];
eval("prod", Stack)        -> [lists:foldl(fun (X,Y) -> X*Y end, 1, Stack)];
eval("+", [N1,N2|Stack])   -> [N2+N1|Stack];
eval("-", [N1,N2|Stack])   -> [N2-N1|Stack];
eval("*", [N1,N2|Stack])   -> [N2*N1|Stack];
eval("/", [N1,N2|Stack])   -> [N2/N1|Stack];
eval("rem", [N1,N2|Stack]) -> [N2 rem N1|Stack];
eval("div", [N1,N2|Stack]) -> [N2 div N1|Stack];
eval("trunc", [N|Stack])   -> [trunc(N)|Stack];
eval("round", [N|Stack])   -> [round(N)|Stack];
eval("^", [N1,N2|Stack])   -> [lib:pow(N2,N1)|Stack];
eval("exp", [N|Stack])     -> [math:exp(N)|Stack];
eval("sqrt", [N|Stack])    -> [math:sqrt(N)|Stack];
eval("ln", [N|Stack])      -> [math:log(N)|Stack];
eval("log", [N|Stack])     -> [math:log10(N)|Stack];
eval("sin", [N|Stack])     -> [math:sin(N)|Stack];
eval("cos", [N|Stack])     -> [math:cos(N)|Stack];
eval("tg", [N|Stack])      -> [math:tan(N)|Stack];
eval("ctg", [N|Stack])     -> [1/math:tan(N)|Stack];
eval("asin", [N|Stack])    -> [math:asin(N)|Stack];
eval("acos", [N|Stack])    -> [math:acos(N)|Stack];
eval("atg", [N|Stack])     -> [math:atan(N)|Stack];
eval("actg", [N|Stack])    -> [math:atan(1/N)|Stack];
eval("=", [N1,N2|Stack])   -> [N2 == N1|Stack];
eval("==", [N1,N2|Stack])  -> [N2 =:= N1|Stack];
eval("/=", [N1,N2|Stack])  -> [N2 /= N1|Stack];
eval("=/=", [N1,N2|Stack]) -> [N2 =/= N1|Stack];
eval("<", [N1,N2|Stack])   -> [N2 < N1|Stack];
eval("=<", [N1,N2|Stack])  -> [N2 =< N1|Stack];
eval(">", [N1,N2|Stack])   -> [N2 > N1|Stack];
eval(">=", [N1,N2|Stack])  -> [N2 >= N1|Stack];
eval("true", Stack)        -> [true|Stack];
eval("false", Stack)       -> [false|Stack];
eval("not", [X|Stack])     -> [not X|Stack];
eval("&", Stack)           -> [lists:foldl(fun (X,Y) -> X and Y end, true, Stack)];
eval("v", Stack)           -> [lists:foldl(fun (X,Y) -> X or Y end, false, Stack)];
eval("and", [N1,N2|Stack]) -> [N2 and N1|Stack];
eval("or", [N1,N2|Stack])  -> [N2 or N1|Stack];
eval("xor", Stack)         -> [not(lists:foldl(fun (X,Y) -> X and Y end, true, Stack))];
eval(X, Stack)             -> [read(X)|Stack].

%% преобразуем токен в число
read(X) ->
    case string:to_float(X) of
        {error, no_float} -> list_to_integer(X);
        {F, _}            -> F
    end.
