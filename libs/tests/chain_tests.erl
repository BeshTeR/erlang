-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля chain
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(to_string, 1) ->
    [
    {[{1,[],[]}],          "1"},
    {[{2,[5,12],[]}],      "2+1/5+1/12"},
    {[{3,[5,12],[2,3,4]}], "3+1/5+1/12+{1/2+1/3+1/4}"}
    ];

tests(make, 2) ->
    [
    {[0,[]],      {0,[],[]}},
    {[0,[1]],     {1,[],[]}},
    {[3,[1]],     {4,[],[]}},
    {[3,[3,1]],   {3,[4],[]}},
    {[1,[2,3,1]], {1,[2,4],[]}},
    {[1,[2,3]],   {1,[2,3],[]}},
    {[0.3,[2,3]], {error, bad_format}},
    {[3, {2,3}],  {error, bad_format}}
    ];

tests(split, 1) ->
    [
    {[{1,[2,3],[4,5,6,7]}],   {1,[2,3],[4,5,6,7]}},
    {[{1,[2,3],[]}],          {1,[2,3],[]}},
    {[{1,[],[4,5,6,7]}],      {1,[],[4,5,6,7]}},
    {[{1,[],[]}],             {1,[],[]}},
    {[{1,[1],[4,5,6,7]}],     {1,[1],[4,5,6,7]}},
    {[{1,[2,3,1],[4,5,6,7]}], {1,[2,3,1],[4,5,6,7]}},
    {[{1,[2,3,1],[1]}],       {1,[2,3,1],[1]}},
    {[{1,[2,3],[1]}],         {1,[2,3],[1]}},
    {[{1,[1],[1]}],           {1,[1],[1]}},
    {[{1,[1],[2,1]}],         {1,[1],[2,1]}},
    {[{1,[2,1],[2]}],         {1,[2,1],[2]}},
    {[{0,[2,1],[2]}],         {0,[2,1],[2]}},
    {[{1,[2,1],[0,2]}],       {error, bad_format}},
    {[{-1,[2,1],[1,2]}],      {error, bad_format}},
    {[{1,[-2,1],[1,2]}],      {error, bad_format}},
    {[atom],                  {error, bad_format}},
    {[12],                    {error, bad_format}},
    {[{1}],                   {error, bad_format}},
    {[[1,2,3]],               {error, bad_format}},
    {[[]],                    {error, bad_format}}
    ];

tests(make, 3) ->
    [
    {[1,[2,3],[4,5,6,7]],    {1,[2,3],[4,5,6,7]}},
    {[1,[2,3],[]],           {1,[2,3],[]}},
    {[1,[],[4,5,6,7]],       {1,[],[4,5,6,7]}},
    {[1,[],[]],              {1,[],[]}},
    {[-1,[2,3],[4,5,6,7]],   {error, bad_format}},
    {[1,[atom,3],[4,5,6,7]], {error, bad_format}},
    {[1,[2,3],[4,5,6,0]],    {error, bad_format}}
    ];

tests(is_chain, 1) ->
    [
    {[{1,[],[]}],              true},
    {[{2,[5,12],[]}],          true},
    {[{3,[5,12],[2,3,4]}],     true},
    {[{3,[],[2,3,4]}],         true},
    {[{-1,[2,3],[4,5,6,7]}],   false},
    {[{1,[atom,3],[4,5,6,7]}], false},
    {[{1,[2,3],[4,5,6,0]}],    false},
    {[{1,[0,3],[4,5,6,2]}],    false},
    {[{1,[-2,3],[4,5,6,2]}],   false},
    {[{0,[2,3],[4,5,6,2]}],    true},
    {[{2,[2,3],[4,atom,6,2]}], false},
    {[{atom,[2,3],[4,5,6,2]}], false},
    {[{1,[2,3]}],              false},
    {[{1}],                    false},
    {[atom],                   false}
    ];

tests(is_infinity, 1) ->
    [
    {[{1,[],[]}],              false},
    {[{2,[5,12],[]}],          false},
    {[{3,[5,12],[2,3,4]}],     true},
    {[{3,[],[2,3,4]}],         true},
    {[{-1,[2,3],[4,5,6,7]}],   false},
    {[{1,[atom,3],[4,5,6,7]}], false},
    {[{1,[2,3],[4,5,6,0]}],    false},
    {[{1,[0,3],[4,5,6,2]}],    false},
    {[{1,[-2,3],[4,5,6,2]}],   false},
    {[{0,[2,3],[4,5,6,2]}],    true},
    {[{2,[2,3],[4,atom,6,2]}], false},
    {[{atom,[2,3],[4,5,6,2]}], false},
    {[{1,[2,3]}],              false},
    {[{1}],                    false},
    {[atom],                   false}
    ];

tests(to_rat, 1) ->
    [
    ];

tests(to_float, 1) ->
    [
    ];

tests(from_natural, 1) ->
    [
    {[3],    {3, [], []}},
    {[0],    {0, [], []}},
    {[-1],   {error, bad_format}},
    {[atom], {error, bad_format}}
    ];

tests(from_rat, 1) ->
    [
    ];

tests(from_float, 1) ->
    [
    ];

tests(depth, 1) ->
    [
    {[{3,[],[]}],       0},
    {[{3,[22],[]}],     1},
    {[{3,[1,2],[]}],    2},
    {[{3,[],[1]}],      infinity},
    {[{3,[2,3],[1,2]}], infinity}
    ];

tests(depth, 2) ->
    [
    {[0, {0,[1,2,3],[4,5,6,7]}], {0,[],[]}},
    {[1, {0,[1,2,3],[4,5,6,7]}], {1,[],[]}},
    {[2, {0,[1,2,3],[4,5,6,7]}], {0,[1,2],[]}},
    {[3, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3],[]}},
    {[4, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3,4],[]}},
    {[5, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3,4,5],[]}},
    {[6, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3,4,5,6],[]}},
    {[7, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3,4,5,6,7],[]}},
    {[8, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3,4,5,6,7,4],[]}},
    {[9, {0,[1,2,3],[4,5,6,7]}], {0,[1,2,3,4,5,6,7,4,5],[]}},
    {[3, {0,[],[]}],             {0,[],[]}},
    {[0, {5,[],[1]}],            {5,[],[]}},
    {[1, {5,[],[1]}],            {6,[],[]}},
    {[2, {5,[],[1]}],            {5,[2],[]}},
    {[3, {5,[],[1]}],            {5,[1,2],[]}},
    {[4, {5,[],[1]}],            {5,[1,1,2],[]}},
    {[0, {5,[7],[3]}],           {5,[],[]}},
    {[1, {5,[7],[3]}],           {5,[7],[]}},
    {[2, {5,[7],[3]}],           {5,[7,3],[]}},
    {[3, {5,[7],[3]}],           {5,[7,3,3],[]}},
    {[4, {5,[7],[3]}],           {5,[7,3,3,3],[]}},
    {[3, {5,[1],[3]}],           {5,[1,3,3],[]}},
    {[2, {5,[7],[1]}],           {5,[8],[]}}
    ];

tests(nth, 2) ->
    [
    {[0, {0,[1,2,3],[4,5,6,7]}], 0},
    {[1, {0,[1,2,3],[4,5,6,7]}], 1},
    {[2, {0,[1,2,3],[4,5,6,7]}], 2},
    {[3, {0,[1,2,3],[4,5,6,7]}], 3},
    {[4, {0,[1,2,3],[4,5,6,7]}], 4},
    {[5, {0,[1,2,3],[4,5,6,7]}], 5},
    {[8, {0,[1,2,3],[4,5,6,7]}], 4},
    {[9, {0,[1,2,3],[4,5,6,7]}], 5},
    {[11,{0,[1,2,3],[4,5,6,7]}], 7},
    {[0, {2,[],[]}],             2},
    {[1, {2,[],[]}],             undefined},
    {[2, {2,[],[]}],             undefined},
    {[2, {2,[1,5],[]}],          5},
    {[3, {2,[1,5],[]}],          undefined},
    {[4, {2,[1,5],[]}],          undefined},
    {[3, {7,[],[1,2]}],          1},
    {[2, {7,[],[1,2]}],          2},
    {[1, {7,[],[1,2]}],          1},
    {[0, {7,[],[1,2]}],          7},
    {[4, {7,[],[1,2]}],          2}
    ];

tests(_, _) ->
    [].
