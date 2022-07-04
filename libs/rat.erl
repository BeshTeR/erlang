%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с рациональными числами
%%% @end
%%% ----------------------------------------------------------------------------
-module(rat).

%% API
-export([make/2, numerator/1, denominator/1, zero/0, one/0, mult/2, division/2, rev/1,  add/2, sub/2, pow/2, is_rational/1, is_zero/1, is_negativ/1]).
-export([is_positiv/1, equal/2, less/2, greq/2, to_string/1, to_float/1, split/1, integer_to_rat/1, float_to_rat/2, is_full/1]).

%% Tests
-include("tests/rat_tests.erl").

%% Тип - рациональное число
-type rational() :: {integer(), pos_integer()}.

%% -----------------------------------------------------------------------------
%% @doc Конструктор рационального числа
%% @end
%% -----------------------------------------------------------------------------
-spec make(Num, Den) -> Return when
    Num    :: integer(),
    Den    :: integer(),
    Return :: rational().

make(Num, Den) when is_integer(Num), is_integer(Den), Den /= 0 ->
    GCD = lib:gcd(Num, Den),
    {lib:sign(Den)*(Num div GCD), abs(Den) div GCD}.

%% -----------------------------------------------------------------------------
%% @doc Числитель рационального числа
%% @end
%% -----------------------------------------------------------------------------
-spec numerator(R) -> Return when
    R      :: rational(),
    Return :: integer().

numerator({Num, _}) ->
    Num.

%% -----------------------------------------------------------------------------
%% @doc Знаменатель рационального числа
%% @end
%% -----------------------------------------------------------------------------
-spec denominator(R) -> Return when
    R      :: rational(),
    Return :: pos_integer().

denominator({_, Den}) ->
    Den.

%% -----------------------------------------------------------------------------
%% @doc Ноль
%% @end
%% -----------------------------------------------------------------------------
-spec zero() -> Return when
    Return :: rational().

zero() ->
    make(0,1).

%% -----------------------------------------------------------------------------
%% @doc Единица
%% @end
%% -----------------------------------------------------------------------------
-spec one() -> Return when
    Return :: rational().

one() ->
    make(1,1).

%% -----------------------------------------------------------------------------
%% @doc Преобразует целое число в рациональное
%% @end
%% -----------------------------------------------------------------------------
-spec integer_to_rat(Int) -> Return when
    Int    :: integer(),
    Return :: rational().

integer_to_rat(Int) when is_integer(Int) ->
    make(Int, 1).

%% -----------------------------------------------------------------------------
%% @doc Преобразует вещественное число в рациональное (учитывая N знаков после запятой)
%% @end
%% -----------------------------------------------------------------------------
-spec float_to_rat(Float, N) -> Return when
    Float  :: float(),
    N      :: non_neg_integer(),
    Return :: rational().

float_to_rat(Float, N) when is_float(Float), is_integer(N), N > 0 ->
    Z = trunc(lib:pow(10, N)),
    make(round(Float*Z), Z).

%% -----------------------------------------------------------------------------
%% @doc Разбивает рациональное число на целую и дробную части
%% @end
%% -----------------------------------------------------------------------------
-spec split(R) -> Return when
    R      :: rational(),
    Return :: {integer(), pos_integer()}.

split(R) ->
    case is_zero(R) of
        true ->
            {0, zero()};
        false ->
            Num = numerator(R),
            Den = denominator(R),
            {Num div Den, make(Num rem Den, Den)}
    end.

%% -----------------------------------------------------------------------------
%% @doc Число обратное данному
%% @end
%% -----------------------------------------------------------------------------
-spec rev(R) -> Return when
    R      :: rational(),
    Return :: rational().

rev(R) ->
    make(denominator(R), numerator(R)).

%% -----------------------------------------------------------------------------
%% @doc Умножение рациональных чисел
%% @end
%% -----------------------------------------------------------------------------
-spec mult(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: rational().

mult(R1, R2) ->
    make(numerator(R1)*numerator(R2), denominator(R1)*denominator(R2)).

%% -----------------------------------------------------------------------------
%% @doc Деление рациональных чисел
%% @end
%% -----------------------------------------------------------------------------
-spec division(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: rational().

division(R1, R2) ->
    make(numerator(R1)*denominator(R2), numerator(R2)*denominator(R1)).

%% -----------------------------------------------------------------------------
%% @doc Сложение рациональных чисел
%% @end
%% -----------------------------------------------------------------------------
-spec add(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: rational().

add(R1, R2) ->
    make(numerator(R1)*denominator(R2)+numerator(R2)*denominator(R1), denominator(R1)*denominator(R2)).

%% -----------------------------------------------------------------------------
%% @doc Вычитание рациональных чисел
%% @end
%% -----------------------------------------------------------------------------
-spec sub(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: rational().

sub(R1, R2) ->
    make(numerator(R1)*denominator(R2)-numerator(R2)*denominator(R1), denominator(R1)*denominator(R2)).

%% -----------------------------------------------------------------------------
%% @doc Возведение рационального числа в целую степень
%% @end
%% -----------------------------------------------------------------------------
-spec pow(R, N) -> Return when
    R      :: rational(),
    N      :: integer(),
    Return :: rational().

pow(R, N) when is_integer(N) ->
    case N >= 0 of
        true  -> make(lib:pow(numerator(R), N), lib:pow(denominator(R), N));
        false -> make(lib:pow(denominator(R), -N), lib:pow(numerator(R), -N))
    end.

%% -----------------------------------------------------------------------------
%% @doc Аргумент - рациональное число?
%% @end
%% -----------------------------------------------------------------------------
-spec is_rational(X) -> Return when
    X      :: any(),
    Return :: boolean().

is_rational({N, D}) when is_integer(N), is_integer(D), D > 0 ->
    true;

is_rational(_) ->
    false.

%% -----------------------------------------------------------------------------
%% @doc Число равно нулю?
%% @end
%% -----------------------------------------------------------------------------
-spec is_zero(R) -> Return when
    R      :: rational(),
    Return :: boolean().

is_zero(R) ->
    numerator(R) == 0.

%% -----------------------------------------------------------------------------
%% @doc Число меньше нуля?
%% @end
%% -----------------------------------------------------------------------------
-spec is_negativ(R) -> Return when
    R      :: rational(),
    Return :: boolean().

is_negativ(R) ->
    numerator(R) < 0.

%% -----------------------------------------------------------------------------
%% @doc Число больше нуля?
%% @end
%% -----------------------------------------------------------------------------
-spec is_positiv(R) -> Return when
    R      :: rational(),
    Return :: boolean().

is_positiv(R) ->
    numerator(R) > 0.

%% -----------------------------------------------------------------------------
%% @doc Число целое?
%% @end
%% -----------------------------------------------------------------------------
-spec is_full(R) -> Return when
    R      :: rational(),
    Return :: boolean().

is_full(R) ->
    denominator(R) == 1.

%% -----------------------------------------------------------------------------
%% @doc Равенство двух рациональных чисел
%% @end
%% -----------------------------------------------------------------------------
-spec equal(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: boolean().

equal(R1, R2) ->
    numerator(R1)*denominator(R2) == numerator(R2)*denominator(R1).

%% -----------------------------------------------------------------------------
%% @doc Меньше?
%% @end
%% -----------------------------------------------------------------------------
-spec less(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: boolean().

less(R1, R2) ->
    numerator(R1)*denominator(R2) < numerator(R2)*denominator(R1).

%% -----------------------------------------------------------------------------
%% @doc Больше?
%% @end
%% -----------------------------------------------------------------------------
-spec greq(R1, R2) -> Return when
    R1     :: rational(),
    R2     :: rational(),
    Return :: boolean().

greq(R1, R2) ->
    numerator(R1)*denominator(R2) > numerator(R2)*denominator(R1).

%% -----------------------------------------------------------------------------
%% @doc Преобразование рациаонального числа в строку
%% @end
%% -----------------------------------------------------------------------------
-spec to_string(R) -> Return when
    R      :: rational(),
    Return :: string().

to_string(R)->
    integer_to_list(numerator(R)) ++
    case not is_full(R) of
        true  -> "/" ++ integer_to_list(denominator(R));
        false -> ""
    end.

%% -----------------------------------------------------------------------------
%% @doc Преобразование рационального числа в вещественное
%% @end
%% -----------------------------------------------------------------------------
-spec to_float(R) -> Return when
    R      :: rational(),
    Return :: string().

to_float(R) ->
    numerator(R) / denominator(R).
