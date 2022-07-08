-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля lib
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(gcd, 2) ->
    [
    {[0, 0],              0},
    {[1, 12],             1},
    {[-4, 6],             2},
    {[11, -7],            1},
    {[24, 30],            6},
    {[123, 54],           3},
    {[54, 246],           6},
    {[14, 78],            2},
    {[156, 28],           4},
    {[6, 6],              6},
    {[3, 1],              1},
    {[0, 9],              9},
    {[12, 12],            12},
    {[-10, 0],            10},
    {[107, 41],           1},
    {[987654321, 123],    3},
    {[99990001, 5882353], 1}
    ];

tests(sign, 1) ->
    [
    {[1],     1},
    {[0],     0},
    {[0.0],   0},
    {[0.1],   1},
    {[-3],    -1},
    {[-12.9], -1}
    ];

tests(pow, 2) ->
    [
    {[2, 10],   1024},
    {[9, 2],    81},
    {[0, 5],    0},
    {[5, 1],    5},
    {[0, -0],   1},
    {[1, -1],   1.0},
    {[10, -3],  0.001},
    {[1, 50],   1},
    {[2.0, 3],  8.0},
    {[2.0, -1], 0.5},
    {[1, -1],   1.0},
    {[5, 20],   95367431640625},
    {[6, 30],   221073919720733357899776},
    {[2, 400],  2582249878086908589655919172003011874329705792829223512830659356540647622016841194629645353280137831435903171972747493376}
    ];

tests(fac, 1) ->
    [
    {[0],  1},
    {[1],  1},
    {[2],  2},
    {[6],  720},
    {[20], 2432902008176640000},
    {[35], 10333147966386144929666651337523200000000},
    {[85], 281710411438055027694947944226061159480056634330574206405101912752560026159795933451040286452340924018275123200000000000000000000}
    ];

tests(id, 1) ->
    [
    {[123],               123},
    {[[res,{ok,"test"}]], [res,{ok,"test"}]},
    {[{<<12345>>,name}],  {<<12345>>,name}},
    {[atom],              atom}
    ];

tests(map, 2) ->
    [
    {[fun id/1, [1,2,3]], [1,2,3]},
    {[fun(_) -> 1 end, [1,2,3]], [1,1,1]},
    {[fun(_) -> false end, [1,2,3]], [false,false,false]},
    {[fun(X) -> X*X end, []], []},
    {[fun(X) -> X*X end, [5]], [25]},
    {[fun(X) -> X*X end, [1,2,3]], [1,4,9]}
    ];

tests(filter, 2) ->
    [
    {[fun(_) -> false end, [1,2,3]], []},
    {[fun(_) -> true end, [1,2,3]], [1,2,3]},
    {[fun(X) -> X rem 2 /= 0 end, []], []},
    {[fun(X) -> X rem 2 /= 0 end, [5]], [5]},
    {[fun(X) -> X rem 2 /= 0 end, [1,2,3]], [1,3]},
    {[fun(X) -> X rem 2 == 0 end, [5]], []},
    {[fun(X) -> X rem 2 == 0 end, [1,2,3]], [2]}
    ];

tests(for, 4) ->
    [
    {[2,1,2, fun id/1], []},
    {[2,6,2, fun(X) -> X*X end], [4,16,36]},
    {[2,7,2, fun(X) -> X*X end], [4,16,36]}
    ];

tests(for, 3) ->
    [
    {[2,1, fun id/1], []},
    {[2,6, fun(X) -> X*X end], [4,9,16,25,36]},
    {[2,7, fun(X) -> X*X end], [4,9,16,25,36,49]}
    ];

tests(for, 2) ->
    [
    {[1, fun id/1], [1]},
    {[6, fun(X) -> X*X end], [1,4,9,16,25,36]},
    {[7, fun(X) -> X*X end], [1,4,9,16,25,36,49]}
    ];

tests(_, _) ->
    [].
