-module(compare).
-export([all/1,any/1,sub/1,list/1,choose/1,apply/6,refine/4,permute/4]).
%%% a permutation is a list
%%% a partition is list of sets specifying a set of permutations
%%% a set refines a partition
%% return all permutations of a partition
all([{flat,Set}|Tail]) ->
    Lists = set:lists({flat,Set}),
    Fun = fun(Left,Right) ->
        Left++Right end,
    lists:map(Fun,lambda:cross(Lists,all(Tail)));
all([]) ->
    [].
%% return any permutation of a partition
any(List) ->
    Fun = fun({flat,Set}) ->
        set:list({flat,Set}) end,
    lists:append(lists:map(Fun,List)).
%% sub refines a partition by a set, returning a partition specifying a subset of the permutations specified by given
sub(Part,{flat,Set}) ->
    Fun0 = fun({flat,Sub}) ->
        [set:intersection({flat,Sub},{flat,Set}),set:difference({flat,Sub},{flat,Set})] end,
    Fun1 = fun({flat,[]}) -> false; ({flat,_}) -> true end,
    lists:filter(Fun1,lists:append(lists:map(Fun0,Part))).
%% list returns all tuples of boundary region side permutation from result of permute
list([{Perm,Part,Side}|Tail]) ->
    Fun = fun(Regs) ->
        {Perm,Regs,Side} end,
    lists:map(Fun,all(Part))++list(Tail);
list([]) ->
    [].
%% choose returns one tuple of boundary region side permutation from result of permute
choose([{Perm,Part,Side}|Tail]) ->
    {Perm,any(Part),Side}.
%% apply returns space with boundaries regions sides permuted so side_t compares
apply({side_t,S_st},{boundaries_f,S_bf},{regions_f,S_rf},Boundaries,Regions,Sides) ->
    Count0 = set:tree(set:counti(Boundaries)),
    Count1 = set:tree(set_counti(Regions)),
    Fun0 = fun(Perm) ->
        set:tree(set:counti(Perm)) end,
    Count2 = set:tree(set:counti(lists:map(Fun0,Sides))),
    Fun1 = fun(Boundary,Region) ->
        Count = set:get(Count2,Boundary),
        Bound = set:get(Count0,Boundary),
        Reg = set:get(Count1,Region),
        Map = set:get({tree,S_st},Bound),
        Side = set:get({tree,Map},Reg),
        set:get(Count,Side) end,
    {side_f,lambda:tabulate(S_bf,S_rf,Fun1)}.
%% refine returns list of sets partitioned from given by given
refine({half_tf,S_htf},Bound,Part,Sides) ->
    Fun0 = fun({flat,Side},Acc) ->
        sub(Acc,space:half({half_tf,S_htf},Side,Bound)) end,
    List = lists:fold(Fun,Sides,Part),
    Fun1 = fun({flat,Set}) ->
        set:length({flat,Set}) end,
    Score = lists:map(Fun1,List),
    {Score,List}.
%% permute returns list of tuple of boundary permutation, region partition, side permutations
permute({half_tf,S_htf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) ->
    {flat,List} = set:lists({flat,S_pf}),
    Count0 = erlang:length(List),
    Count3 = erlang:length(S_bf),
    Fixed = {{half_tf,S_htf},List,Count0},
    Candidates = [{[],[{flat,S_rf}],[],S_bf,Count3}],
    Survivors = permute_r(Fixed,[],0,0,{},0,0,{},1,Count3,Candidates),
    Fun = fun({Perm,Part,Side,[],0}) ->
        {lists:reverse(Perm),Part,lists:reverse(Side)} end,
    lists:map(Fun,Survivors).
%% Fixed is half_tf, list of side permutations List, length of list Count0
%% Survivors and Candidates are lists and Clone is single of form {Perm,Part,Side,Pool,Count3}
%% Perm is permutation of boundaries in reverse order; Part is partition of regions
%% Side is list of side permutations corresponding to Perm; Pool is remaining boundaries to add to Perm
%% Patient is tuple of form {Perm,Part,Side,Left,Right,Pool,Count3,Tail}
%% Left is skipped boundaries from Pool; Right is current boundary and to be skipped boundaries from Pool
%% Tail is current and remaining side permutations for head of Right
%% Count0 is length of Tail in Patient; Count1 is length of Right in Patient
%% Count2 is length of Candidates; Count3 is length of Pool in Candidates
%% a score is a list of the set lengths in a partition
%% Max is score for tuples in Survivors and Score is score for Clone
%% Candidates is list of tuples of form {Perm,Part,Pool,Count3}
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score > 0 and Score == Max ->
    permute_r(Fixed,[Clone|Survivors],Max,0,Clone,Count0,Count1,Patient,Count2,Count3,Candidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score > 0 and Score > Max ->
    permute_r(Fixed,[Clone],Score,0,Clone,Count0,Count1,Patient,Count2,Count3,Candidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score > 0 and Score < Max ->
    permute_r(Fixed,Survivors,Max,0,Clone,Count0,Count1,Patient,Count2,Count3,Candidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score == 0 and Count0 > 0 ->
    {NewScore,NewClone,NewPatient} = permute_r1(Fixed,Patient),
    permute_r(Fixed,Survivors,Max,NewScore,NewClone,Count0-1,Count1,NewPatient,Count2,Count3,Candidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score == 0 and Count0 == 0 and Count1 > 0 ->
    {NewCount0,NewPatient} = permute_r2(Fixed,Patient),
    permute_r(Fixed,Survivors,Max,Score,Clone,NewCount0,Count1-1,NewPatient,Count2,Count3,Candidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score == 0 and Count0 == 0 and Count1 == 0 and Count2 > 0 ->
    {NewCount0,NewCount1,NewPatient,NewCandidates} = permute_r3(Fixed,Candidates),
    permute_r(Fixed,Survivors,Max,Score,Clone,NewCount0,NewCount1,NewPatient,Count2-1,Count3,NewCandidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score == 0 and Count0 == 0 and Count1 == 0 and Count2 == 0 and Count3 > 0 ->
    {NewSurvivors,NewMax,NewCount2,NewCandidates} = permute_r4(Fixed,Survivors),
    permute_r(Fixed,NewSurvivors,NewMax,Score,Clone,Count0,Count1,Patient,NewCount2,Count3-1,NewCandidates);
permute_r(Fixed,Survivors,Max,Score,Clone,Count0,Count1,Patient,Count2,Count3,Candidates) when
    Score == 0 and Count0 == 0 and Count1 == 0 and Count2 == 0 and Count3 == 0 ->
    Survivors.
%% {NewScore,NewClone,NewPatient} = permute_r1(Fixed,Patient),
permute_r1({{half_tf,S_htf},List,Count0},{Perm,Part,Side,Left,[Bound|Right],Count1,Pool,Count3,[Head|Tail]}) ->
    {Score,Refine} = refine({half_tf,S_htf},Bound,Part,Head),
    {Score,{[Bound|Perm],Refine,[Head|Side],Pool,Count3},{Perm,Part,Side,Left,[Bound|Right],Count1,Pool,Count3,Tail}}.
%% {NewCount0,NewPatient} = permute_r2(Fixed,Patient),
permute_r2({{half_tf,S_htf},List,Count0},{Perm,Part,Side,Left,[Bound,Middle|Right],Count1,Pool,Count3,Tail}) ->
    Pool = Left++[Bound],
    {Count0,{Perm,Part,Side,Pool,[Middle|Right],Count1-1,Pool++Right,Count3,List}};
permute_r2({{half_tf,S_htf},List,Count0},{Perm,Part,Side,Left,[Bound],Count1,Pool,Count3,Tail}) ->
    {0,{}}.
%% {NewCount0,NewCount1,NewPatient,NewCandidates} = permute_r3(Fixed,Candidates),
permute_r3({{half_tf,S_htf},List,Count0},[{Perm,Part,Side,[Head|Pool],Count3}|Candidates]) ->
    {Count0,Count3,{Perm,Part,Side,[],[Head|Pool],Count3,Pool,Count3-1,List},Candidates}.
%% {NewSurvivors,NewMax,NewCount2,NewCandidates} = permute_r4(Fixed,Survivors),
permute_r4({{half_tf,S_htf},List,Count0},Survivors) ->
    {[],0,erlang:length(Survivors),Survivors}.
