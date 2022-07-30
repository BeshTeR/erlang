-export([tests/2]).

%% -----------------------------------------------------------------------------
%% @doc Данные для тестов модуля chain
%% @end
%% -----------------------------------------------------------------------------
-spec tests(Fun, Arity) -> Return when
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: [{[any()], any()}].

tests(make, 2) ->
    [
    {[0,[]],       {0,[],[]}},
    {[0,[1]],      {1,[],[]}},
    {[3,[1]],      {4,[],[]}},
    {[3,[3,1]],    {3,[4],[]}},
    {[1,[2,3,1]],  {1,[2,4],[]}},
    {[1,[2,3]],    {1,[2,3],[]}},
    {[0.3,[2,3]],  {error, bad_format}},
    {[3, {2,3}],   {error, bad_format}},
    {[0,[10],[]],  {0,[10],[]}},
    {[5,[2,2],[]], {5,[2,2],[]}}
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

tests(to_string, 1) ->
    [
    {[{1,[],[]}],          "1"},
    {[{2,[5,12],[]}],      "2+1/5+1/12"},
    {[{3,[5,12],[2,3,4]}], "3+1/5+1/12+{1/2+1/3+1/4}"}
    ];

tests(to_list, 1) ->
    [
    {[{1,[],[]}],          [1]},
    {[{2,[5,12],[]}],      [2,5,12]},
    {[{3,[5,12],[2,3,4]}], undefined}
    ];

tests(to_rat, 1) ->
    [
    {[{0,[],[]}],          rat:make(0,1)},
    {[{1,[],[]}],          rat:make(1,1)},
    {[{1,[1],[]}],         rat:make(2,1)},
    {[{1,[1,1],[]}],       rat:make(3,2)},
    {[{1,[1,1,1],[]}],     rat:make(5,3)},
    {[{1,[1,1,1,1],[]}],   rat:make(8,5)},
    {[{1,[1,1,1,1,1],[]}], rat:make(13,8)},
    {[{0,[10],[]}],        rat:make(1,10)},
    {[{5,[2,2],[]}],       rat:make(27,5)},
    {[{5,[2],[]}],         rat:make(11,2)}
    ];

tests(to_float, 1) ->
    [
    {[{1,[],[]}],        1.0},
    {[{1,[1],[]}],       2.0},
    {[{1,[1,1],[]}],     1.5},
    {[{1,[1,1,1,1],[]}], 1.6},
    {[{0,[1],[]}],       1.0},
    {[{0,[10],[]}],      0.1},
    {[{5,[2,2],[]}],     5.4},
    {[{5,[2],[]}],       5.5}
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
    {[rat:make(0,1)]},   {0,[],[]},
    {[rat:make(1,1)]},   {1,[],[]},
    {[rat:make(3,1)]},   {3,[],[]},
    {[rat:make(3,2)]},   {1,[2],[]},
    {[rat:make(2,3)]},   {0,[1,2],[]},
    {[rat:make(5,2)]},   {1,[2],[]},
    {[rat:make(5,3)]},   {1,[1,2],[]},
    {[rat:make(5,3)]},   {1,[1,2],[]},
    {[rat:make(11,2)]},  {5,[2],[]},
    {[rat:make(27,5)]},  {5,[2,2],[]},
    {[rat:make(13,8)]},  {1,[1,1,2],[]},
    {[rat:make(21,13)]}, {1,[1,1,1,2],[]},
    {[rat:make(34,21)]}, {1,[1,1,1,1,2],[]},
    {[rat:make(55,34)]}, {1,[1,1,1,1,1,2],[]}
    ];

tests(from_float, 2) ->
    [
    {[1.5,1],  {1,[2],[]}},
    {[1.5,0],  {1,[],[]}},
    {[0.0,1],  {0,[],[]}},
    {[0.01,0], {0,[],[]}},
    {[0.01,1], {0,[],[]}},
    {[0.01,2], {0,[100],[]}},
    {[0.01,5], {0,[100],[]}},
    {[1.0,1],  {1,[],[]}},
    {[8.0,10], {8,[],[]}},
    {[0.25,2], {0,[4],[]}},
    {[2.5,3],  {2,[2],[]}}
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

tests(equal, 2) ->
    [
    {[{7,[],[]}, {7,[],[]}],                         true},
    {[{1,[],[]}, {7,[],[]}],                         false},
    {[{7,[1,2],[]}, {7,[1,2],[]}],                   true},
    {[{7,[1,2],[]}, {7,[1,3],[]}],                   false},
    {[{7,[1,2],[]}, {7,[2,2],[]}],                   false},
    {[{7,[1,2],[]}, {7,[1,2,3],[]}],                 false},
    {[{7,[1],[]}, {7,[1,2],[]}],                     false},
    {[{0,[1,2],[3,4,5]}, {0,[1,2],[3,4,5]}],         true},
    {[{1,[1,2],[3,4,5]}, {0,[1,2],[3,4,5]}],         false},
    {[{0,[2,2],[3,4,5]}, {0,[1,2],[3,4,5]}],         false},
    {[{0,[1,2],[3,7,5]}, {0,[1,2],[3,4,5]}],         false},
    {[{0,[1,2],[3,4,5]}, {0,[1,2],[3,4]}],           false},
    {[{0,[],[2]}, {0,[2],[2]}],                      true},
    {[{0,[2,3,4],[3,4,5]}, {0,[2,3,4,3,4],[5,3,4]}], true},
    {[{0,[1,2],[3,4]}, {0,[1,2,3],[4,3]}],           true},
    {[{1,[],[1]}, {1,[1],[]}],                       false},
    {[{7,[],[1]}, {7,[],[1]}],                       true},
    {[{1,[],[1]}, {1,[1,1,1],[1]}],                  true}
    ];

tests(greq, 2) ->
    [
    {[{7,[],[]}, {7,[],[]}],           false},
    {[{1,[],[]}, {7,[],[]}],           false},
    {[{7,[],[]}, {1,[],[]}],           true},
    {[{1,[2],[]}, {1,[3],[]}],         true},
    {[{1,[2,3],[]}, {1,[2,3],[]}],     false},
    {[{1,[3],[]}, {1,[2],[]}],         false},
    {[{1,[],[3]}, {1,[],[2]}],         false},
    {[{1,[],[2]}, {1,[],[3]}],         true},
    {[{1,[4],[3]}, {1,[4],[2]}],       true},
    {[{1,[4],[2]}, {1,[4],[3]}],       false},
    {[{1,[2,3],[]}, {1,[2],[]}],       true},
    {[{1,[2],[]}, {1,[2,3],[]}],       false},
    {[{7,[1,2],[]}, {7,[1,2],[]}],     false},
    {[{7,[1,2],[]}, {7,[1,3],[]}],     false},
    {[{1,[2],[]}, {1,[],[2]}],         false},
    {[{1,[5,2],[]}, {1,[5],[2]}],      true},
    {[{1,[],[1]}, {1,[],[]}],          true},
    {[{2,[],[2]}, {2,[],[]}],          true},
    {[{1,[],[1]}, {1,[1],[]}],         false},
    {[{1,[],[1]}, {1,[1,1],[]}],       true},
    {[{1,[],[1]}, {1,[1,1,1],[]}],     false},
    {[{1,[],[1]}, {1,[1,1,1,1],[]}],   true},
    {[{1,[],[1]}, {1,[1,1,1,1,1],[]}], false}
    ];

tests(less, 2) ->
    [
    {[{7,[],[]}, {7,[],[]}],           false},
    {[{1,[],[]}, {7,[],[]}],           true},
    {[{7,[],[]}, {1,[],[]}],           false},
    {[{1,[2],[]}, {1,[3],[]}],         false},
    {[{1,[2,3],[]}, {1,[2,3],[]}],     false},
    {[{1,[3],[]}, {1,[2],[]}],         true},
    {[{1,[],[3]}, {1,[],[2]}],         true},
    {[{1,[],[2]}, {1,[],[3]}],         false},
    {[{1,[4],[3]}, {1,[4],[2]}],       false},
    {[{1,[4],[2]}, {1,[4],[3]}],       true},
    {[{1,[2,3],[]}, {1,[2],[]}],       false},
    {[{1,[2],[]}, {1,[2,3],[]}],       true},
    {[{7,[1,2],[]}, {7,[1,2],[]}],     false},
    {[{7,[1,2],[]}, {7,[1,3],[]}],     true},
    {[{1,[2],[]}, {1,[],[2]}],         true},
    {[{1,[5,2],[]}, {1,[5],[2]}],      false},
    {[{1,[],[1]}, {1,[],[]}],          false},
    {[{2,[],[2]}, {2,[],[]}],          false},
    {[{1,[],[1]}, {1,[1],[]}],         true},
    {[{1,[],[1]}, {1,[1,1],[]}],       false},
    {[{1,[],[1]}, {1,[1,1,1],[]}],     true},
    {[{1,[],[1]}, {1,[1,1,1,1],[]}],   false},
    {[{1,[],[1]}, {1,[1,1,1,1,1],[]}], true}
    ];

tests(under, 2) ->
    [
    {[{1,[],[]},{1,[],[1]}],                        true},
    {[{1,[2],[]},{1,[],[1]}],                       true},
    {[{1,[1,2],[]},{1,[],[1]}],                     true},
    {[{1,[1,1,2],[]},{1,[],[1]}],                   true},
    {[{1,[1,1,1,2],[]},{1,[],[1]}],                 true},
    {[{1,[2,3,4],[5,6,7,8]},{1,[2,3,4],[5,6,7,8]}], true},
    {[{1,[],[]},{1,[2,3,4],[5,6,7,8]}],             true},
    {[{1,[2,3,4],[]},{1,[2,3,4],[5,6,7,8]}],        true},
    {[{1,[],[5,6,7,8]},{1,[2,3,4],[5,6,7,8]}],      false},
    {[{1,[2,3,4],[]},{1,[],[2,3,4]}],               true},
    {[{2,[],[]},{1,[],[1]}],                        false},
    {[{2,[3],[]},{2,[3,5],[]}],                     true},
    {[{2,[3],[]},{2,[],[3,5]}],                     true},
    {[{2,[],[3]},{2,[3,3,3,3,3],[]}],               false},
    {[{2,[3,3,3,3,3],[3]},{2,[],[3]}],              true},
    {[{2,[3,3,3,3,3],[]},{2,[],[3]}],               true}
    ];

tests(mult, 2) ->
    [
    {[{4,[],[]},{3,[],[]}],     {12,[],[]}},
    {[{4,[2],[]},{2,[],[]}],    {9,[],[]}},
    {[{4,[2],[]},{1,[2],[]}],   {6,[1,3],[]}},
    {[{0,[3],[]},{3,[],[]}],    {1,[],[]}},
    {[{0,[1,2],[]},{1,[2],[]}], {1,[],[]}},
    {[{1,[2],[]},{0,[2],[]}],   {0,[1,3],[]}}
    ];

tests(division, 2) ->
    [
    {[{4,[],[]},{3,[],[]}],     {1,[3],[]}},
    {[{4,[2],[]},{2,[],[]}],    {2,[4],[]}},
    {[{4,[2],[]},{1,[2],[]}],   {3,[],[]}},
    {[{0,[3],[]},{3,[],[]}],    {0,[9],[]}},
    {[{0,[1,2],[]},{1,[2],[]}], {0,[2,4],[]}},
    {[{1,[2],[]},{0,[2],[]}],   {3,[],[]}}
    ];

tests(add, 2) ->
    [
    {[{4,[],[]},{3,[],[]}],     {7,[],[]}},
    {[{0,[],[]},{0,[],[]}],     {0,[],[]}},
    {[{4,[2],[]},{2,[],[]}],    {6,[2],[]}},
    {[{4,[2],[]},{1,[2],[]}],   {6,[],[]}},
    {[{0,[3],[]},{3,[],[]}],    {3,[3],[]}},
    {[{0,[1,2],[]},{1,[2],[]}], {2,[6],[]}},
    {[{1,[2],[]},{0,[3],[]}],   {1,[1,5],[]}}
    ];

tests(sub, 2) ->
    [
    {[{4,[],[]},{3,[],[]}],           {1,[],[]}},
    {[{3,[],[]},{4,[],[]}],           {1,[],[]}},
    {[{0,[],[]},{0,[],[]}],           {0,[],[]}},
    {[{1,[2,3,4],[]},{1,[2,3,4],[]}], {0,[],[]}},
    {[{4,[2],[]},{2,[],[]}],          {2,[2],[]}},
    {[{4,[2],[]},{1,[2],[]}],         {3,[],[]}},
    {[{0,[3],[]},{3,[],[]}],          {2,[1,2],[]}},
    {[{3,[],[]},{0,[3],[]}],          {2,[1,2],[]}},
    {[{0,[1,2],[]},{1,[2],[]}],       {0,[1,5],[]}},
    {[{1,[2],[]},{0,[1,2],[]}],       {0,[1,5],[]}},
    {[{1,[2],[]},{0,[3],[]}],         {1,[6],[]}},
    {[{0,[3],[]},{1,[2],[]}],         {1,[6],[]}}
    ];

tests(rev, 1) ->
    [
    {[{1,[],[1]}],        {0,[1],[1]}},
    {[{0,[],[1]}],        {1,[],[1]}},
    {[{0,[1,2,3],[4,5]}], {1,[2,3],[4,5]}},
    {[{7,[1,2,3],[4,5]}], {0,[7,1,2,3],[4,5]}},
    {[{0,[],[4,5]}],      {4,[5],[4,5]}},
    {[{1,[],[4,5]}],      {0,[1],[4,5]}},
    {[{1,[],[]}],         {1,[],[]}},
    {[{5,[],[]}],         {0,[5],[]}},
    {[{5,[],[2]}],        {0,[5],[2]}},
    {[{1,[1],[]}],        {0,[2],[]}}
    ];

tests(_, _) ->
    [].
