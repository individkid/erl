-module(vector).
-export([times/2,over/2,plus/2,minus/2,transpose/3,versor/2,identity/1]).
-export([extract/7,multiply/5,solve/3,intersect/3,construct/2]).
times([Scalar],Vector) ->
    Fun = fun(Elem) ->
        Scalar*Elem end,
    lists:map(Fun,Vector).
over([Scalar],Vector) ->
    Fun = fun(Elem) ->
        Elem/Scalar end,
    lists:map(Fun,Vector).
plus(Vec0,Vec1) ->
    Fun = fun({Elem0,Elem1}) ->
        Elem0+Elem1 end,
    lists:map(Fun,lambda:zip(Vec0,Vec1)).
minus(Vec0,Vec1) ->
    Fun = fun({Elem0,Elem1}) ->
        Elem0-Elem1 end,
    lists:map(Fun,lambda:zip(Vec0,Vec1)).
transpose(Mat,X,Y) ->
    lists:append(extract(Mat,0,1,Y,X,1,Y)).
versor(Index,Dimension) ->
    Fun = fun(I) when I == Index ->
        1.0;
    (I) when I /= Index ->
        0.0 end,
    lists:map(Fun,lists:seq(0,Dimension-1)).
identity(Dimension) ->
    Fun = fun({I,J}) when I == J ->
        1.0;
    ({I,J}) when I /= J ->
        0.0 end,
    lists:map(Fun,lambda:cross(lists:seq(0,Dimension-1),lists:seq(0,Dimension-1))).
extract(List,Phase,Duty,Stride,Length,Period,Duration) ->
    lists:reverse(extract_r([],List,Phase,Duty,Stride,Length,Period,Duration,erlang:length(List),[],[],0,0,0)).
extract_r(_,   [],   _,    _,   _,     _,     _,     _,       _,   _,  _,  _,     _,   _) ->
    %%  List, Keep,Phase,Duty,Stride,Period,Length,Duration,Size,Acc,Arg,Offset,Done,Count
    %% prevent infinite loop if given list is empty
    [];
extract_r(_,    _,   _,    _,   _,     _,     _,     0,       _,   _,  _,  _,     _,   _) ->
    %%  List, Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count
    %% prevent infinite loop if requested to find lists of zero Length
    [];
extract_r(_,    _,   _,    _,   _,     _,     _,     Duration,_    Acc,_,  _,     _,   Count,_) when
    %%  List, Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count,Base
    Count == Duration ->
    %% no more lists to map
    Acc;
extract_r([],Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count) ->
    %% no more elements to consume
    extract_r(Keep,Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count);
extract_r([H|T],Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count) when
    Done == Length ->
    %% accumulate list mapped from list of given Length and start next Period
    extract_r(Keep,Keep,((Phase+Period) rem Size),Duty,Stride,Length,Period,Duration,Size,[Arg|Acc],[],0,0,Count+1);
extract_r([H|T],Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count) when
    Offset >= Phase and ((Offset - Phase) rem Stride) < Duty ->
    %% consume Duty elements from each Stride starting from Phase
    extract_r(T,Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,[H|Arg],Offset+1,Done+1,Count);
extract_r([H|T],Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset,Done,Count) ->
    %% skip elements before Phase or after Duty
    extract_r(T,Keep,Phase,Duty,Stride,Length,Period,Duration,Size,Acc,Arg,Offset+1,Done,Count).
helper(List,Given,Desired,Default) ->
    Extra = lists:map(Default,lists:seq(0,erlang:length(Given)-erlang:length(List))),
    {tree,Map} = set:tree(set:sortk(lambda:zip(Given,List++Extra))),
    lists:map(fun(Index) -> set:get({tree,Map},Index) end,Desired).
helper(Rows,Given,Desired) ->
    helper(Rows,Given,Desired,fun(Index) -> versor(Index,Params+1) end).
multiply(Vec0,Vec1,1,Y,1) -> %% 1*y times y*1 is dot product
    Fun = fun({Elem0,Elem1},Acc) ->
        Elem0*Elem1+Acc end,
    [V0] = extract(Vec0,0,Y,0,Y,0,1),
    [V1] = extract(Vec1,0,Y,0,Y,0,1),
    [lists:fold(Fun,lambda:zip(V0,V1),0)].
multiply(Mat0,Mat1,X,Y,Z) ->
    Fun = fun(Vec0,Vec1) ->
        multiply(Vec0,Vec1,1,Y,1) end,
    M0 = extract(Mat0,0,Y,0,Y,Y,X),
    M1 = columns(Mat1,0,1,Z,Y,1,Z),
    lists:append(lists:map(Fun,lambda:cross(M0,M1))).
solve(Matrix,Augment,Indices,X,Y) ->
    [Row] = rows(Matrix,0,Y,0,Y,0,1),
    [Head|Tail] = Augment,
    Max = fun({Ind,Val},{_,Acc}) when Val > 0.0 andalso Acc > 0.0 andalso Val > Acc -> {Ind,Val};
    ({Ind,Val},{_,Acc}) when Val > 0.0 andalso Acc < 0.0 andalso Val > -Acc -> {Ind,Val};
    ({Ind,Val},{_,Acc}) when Val < 0.0 andalso Acc > 0.0 andalso -Val > Acc -> {Ind,Val};
    ({Ind,Val},{_,Acc}) when Val < 0.0 andalso Acc < 0.0 andalso -Val > -Acc -> {Ind,Val};
    ({_,Val},{Ind,Acc}) -> {Ind,Acc} end,
    {Swap,_} = lists:fold(Max,lambda:zip(lists:seq(0,Y-1),Row),{Y,0.0}),
    {Coeffs,Permute,Params} = fun() when Swap == Y andalso Head \= 0.0 ->
        throw([]);
    () when Swap == Y ->
        [After] = extract(Matrix,Y,(X-1)*Y,0,(X-1)*Y,0,1),
        solve(After,Tail,Indices,X-1,Y);
    () when Swap == 0 ->
        solve_r(Matrix,Augment,Indices,X,Y);
    () ->
        {[LIndex|LIndices],[RIndex|RIndices]} = lists:split(Swap,Indices),
        {[LColumn|LColumns],[RColumn|RColumns]} = lists:split(Swap,extract(Matrix,0,1,Y,X,1,Y)),
        Permuted = [RIndex|LIndices]++[LIndex|RIndices],
        Columns = [RColumn|LColumns]++[LColumn|RColumns]++[Augment],
        Appended = lists:append(extract(lists:append(Columns),0,1,X,Y,1,X)),
        solve_r(Appended,Augment,Permuted,X,Y) end
    ().
solve_r(Matrix,Augment,Indices,X,Y) ->
    [[Denom|Numers]|Columns] = extract(Matrix,0,1,Y,X,1,Y),
    [AugFirstRow|AugSubRows] = extract(lists:append(Columns)++Augment,0,1,X,Y,1,X),
    [FirstRow] = extract(AugFirstRow,0,Y-1,0,Y-1,0,1),
    [Index|SubIndices] = Indices,
    %% divide augmented first row to make first element one
    OneFirstRow = over(Denom,AugFirstRow),
    %% zero out elements after first in first column
    MapList = lambda:zip(AugSubRows,Numers),
    ZeroSubMatrix = lists:append(lists:map(fun({R,N}) -> minus(R,times(N,OneFirstRow)) end,MapList)),
    %% separate augment and matrix
    SubMatrix = lists:append(extract(ZeroSubMatrix,0,Y-1,0,Y-1,Y,X-1)),
    [SubAugment] = extract(ZeroSubMatrix,Y,1,Y,X-1,0,1),
    %% recurse to isolate variables after first
    {SubCoeffs,SubPermute,Params} = solve(SubMatrix,SubAugment,lists:seq(0,Y-2),X-1,Y-1),
    %% permute to match permutation of variables after first
    {tree,RowMap} = set:tree({flat,lambda:zip(lists:seq(0,Y-1),FirstRow)}),
    PermFirstRow = lists:map(fun(Pos) -> set:get({tree,RowMap},Pos) end,SubPermute),
    {tree,IndicesMap} = set:tree({flat,lambda:zip(lists:seq(0,Y-1),SubIndices)}),
    PermSubIndices = lists:map(fun(Count) -> set:get({tree,IndicesMap},Count) end,SubPermute),
    {FirstCoeffNumers,FirstCoeffs} = lists:split(Y-Params-1,PermFirstRow),
    %% extract rows from coefficients for isolated variables
    SubCoeffRows = extract(SubCoeffs,0,Params,0,Params,Params,Y-Params-1),
    %% zero out nonparameter locations in first row
    FoilList = lambda:zip(FirstCoeffNumers,SubCoeffRows),
    ZeroFirstCoeffs = lists:foil(fun(N,R,Acc) -> minus(Acc,times(N,R)) end,FoilList,FirstCoeffs),
    %% append first row to coefficients and first variable index
    Coeffs = ZeroFirstCoeffs++SubCoeffs,
    Permute = [Index|SubPermute],
    {Coeffs,Permute,Params}.
intersect(Square,Indices,Bases) ->
    Length = lists:length(Indices),
    Area = Length*Length,
    Crown = Length*(Length-1),
    Fun = fun(Index) ->
        lists:append(extract(Bases,Index*Area,Area,0,Area,0,1)) end,
    Chosen = lists:map(Fun,Indices),
    [Head|Tail] = lists:append(Chosen),
    Matrix = intersect_r(Square,Indices,Tail,0,0,Head,Length,Area,Crown,[]),
    {Coeffs,Permute,Params} = solve(Matrix,Area,Area),
    Rows = extract(Coeffs,0,Params+1,0,Params+1,Params+1,Area-Params),
    %
    % implicit | Rows | unknowns | Permute | equations
    %
    % 1 0 | a c e | y | 2 | y = az + cx + e
    % 0 1 | b d f | w | 0 | w = bz + dx + f
    %     |       | z | 3 |
    %     |       | x | 1 |
    %
    % 0 1 0 0 | b d f | w | 0 | w = bp + dq + f
    % 0 0 0 1 | 0 1 0 | x | 1 | x =       q 
    % 1 0 0 0 | a c e | y | 2 | y = ap + cq + e
    % 0 0 1 0 | 1 0 0 | z | 3 | z =  p
    %
    Desired = lists:seq(Area-Length,Area-1),
    {helper(Rows,Permute,Desired),Params}.
intersect_r([Hs|Ts],[Hi|Ti],[Hb|Tb],Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column < Crown andalso
    (Column div (Length - 1)) == (Row div Length) andalso
    Hi == (Row rem Length) ->
    intersect_r(Ts,[Hi|Ti],Tb,Row,Column+1,Saved,Length,Area,Crown,[(Hb-Saved+Hs)|Acc]);
intersect_r(S,[Hi|Ti],[Hb|Tb],Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column < Crown andalso
    (Column div (Length - 1)) == (Row div Length) andalso
    Hi /= (Row rem Length) ->
    intersect_r(S,[Hi|Ti],Tb,Row,Column+1,Saved,Length,Area,Crown,[(Hb-Saved)|Acc]);
intersect_r(S,I,B,Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column < Crown andalso
    (Column div (Length - 1)) /= (Row div Length) ->
    intersect_r(S,I,B,Row,Column+1,Saved,Length,Area,Crown,[0.0|Acc]);
intersect_r(S,I,B,Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column >= Crown andalso Column < Area andalso
    (Column rem Length) == (Row rem Length) ->
    intersect_r(S,I,B,Row,Column+1,Saved,Length,Area,Crown,[1.0|Acc]);
intersect_r(S,I,B,Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column >= Crown andalso Column < Area andalso
    (Column rem Length) /= (Row rem Length) ->
    intersect_r(S,I,B,Row,Column+1,Saved,Length,Area,Crown,[0.0|Acc]);
intersect_r(S,I,[Hb|Tb],Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column == Area andalso
    ((Row + 1) rem Length) /= 0 ->
    intersect_r(S,I,Tb,Row+1,0,Hb,Length,Area,Crown,[Saved|Acc]);
intersect_r(S,[_|Ti],[Hb|Tb],Row,Column,Saved,Length,Area,Crown,Acc) when
    Row < Area andalso Column == Area andalso
    ((Row + 1) rem Length) == 0 ->
    intersect_r(S,Ti,Tb,Row+1,0,Hb,Length,Area,Crown,[Saved|Acc]);
intersect_r(_,_,_,_,_,_,_,_,_,Acc) when
    Acc.
construct(Square,Bases) ->
    % choose dimension and square from Bases by finding minimum dimension of Square
    % if h is chosen dimension and the other two are i and j
    % assume x y z from Square correspond to i j h
    % find h by plugging in i and j
    % h0 = ai0 + bj0 + c
    % h1 = ai1 + bj1 + c
    % h2 = ai2 + bj2 + c
    % solve for a b c given that
    % z0 = ax0 + by0 + c
    % z1 = ax1 + by1 + c
    % z2 = ax2 + by2 + c
    %
    % x0 y0 1   a   z0
    % x1 y1 1 * b = z1
    % x2 y2 1   c   z2
    %
    % h0   i0 j0 1   a
    % h1 = i1 j1 1 * b
    % h2   i2 j2 1   c
    % then add h to chosen dimension of chosen square from Bases
    .
