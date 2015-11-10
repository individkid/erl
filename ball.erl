-module(ball).
%% structure is {ids,eqs,id->eq,eq->ids,pool}
%% "find" returns identifiers matching given pattern.
%% "class" returns classes of given identifiers.
%% "ident" returns identifiers of given classes.
%% "get" returns classes directly linked to given class.
%% "put" adds prefix linked identifier with optional equivalence.
%% "same" adds equivalence between given identifiers.
%% "empty" returns empty ball.
-export([find/2,class/2,ident/2,get/2,put/2,put/3]).
-export([same/3,clean/3,sub/2,sup/2,empty/0]).
find(Ball,Pat) ->
    {Idents,_,_,_,_} = Ball,
    Len = erlang:length(Pat),
    Seq = erlang:seq(1,Len),
    Fun = fun(Id) ->
        Fun = fun(Sub) ->
            Left = erlang:element(Sub,Pat),
            Right = erlang:element(sub,Id),
            (Left == wild) or (Left == Right) end,
        lists:all(Fun,Seq) end,
    set:filter(Idents,Fun).
class(Ball,Ids) ->
    {_,_,PerId,_,_} = Ball,
    set:image(PerId,Ids).
ident(Ball,Eqs) ->
    {_,_,_,PerEq,_} = Ball,
    Fam = set:image(PerEq,Eqs),
    set:union_f(Fam).
get(Ball,Class) ->
    {_,Equivs,_,PerEq,_} = Ball,
    Ids = set:get(PerEq,Class),
    Fun = fun(Eq) ->
        Set = set:get(PerEq,Eq),
        Fun = fun(L,R) ->
            PreL = lists:droplast(L),
            PreR = lists:droplast(R),
            (L == R) or (PreL == R) or (L == PreR) end,
        set:foil_any(Ids,Set,Fun) end,
    set:filter(Equivs,Fun).
put(Ball,Ident) ->
    {Idents,Equivs,PerId,PerEq,Pool} = Ball,
    Equiv = set:length(Equivs)+Pool,
    NewIds = set:insert(Idents,Ident),
    NewEqs = set:insert(Equivs,Equiv),
    NewPId = set:insert(PerId,Ident,Equiv),
    Ids = set:single(Ident),
    NewPEq = set:insert(PerEq,Equiv,Ids),
    {NewIds,NewEqs,NewPId,NewPEq,Pool}.
put(Ball,Ident,Other) ->
    {Idents,Equivs,PerId,PerEq,Pool} = Ball,
    Equiv = set:get(PerId,Other),
    NewIds = set:insert(Idents,Ident),
    NewPId = set:insert(PerId,Ident,Equiv),
    Ids = set:get(PerEq,Equiv),
    Set = set:insert(Ids,Ident),
    NewPEq = set:replace(PerEq,Equiv,Set),
    {NewIds,Equivs,NewPId,NewPEq,Pool}.
same(Ball,Ident,Other) ->
    {Idents,Equivs,PerId,PerEq,Pool} = Ball,
    Eq0 = set:get(PerId,Ident),
    Eq1 = set:get(PerId,Other),
    Res = fun(false) ->
        Ids = set:get(PerEq,Eq0),
        Oth = set:get(PerEq,Eq1),
        Set = set:union(Ids,Oth),
        New = set:replace(PerEq,Eq0,Set),
        NewPEq = set:remove(New,Eq1),
        NewEqs = set:remove(Equivs,Eq1),
        Fun = fun(Id,Eq) ->
            Res = fun(true) ->
                Eq0; (false) ->
                Eq end (set:member(Oth,Id)),
            Res end,
        NewPId = set:remap(PerId,Fun),
        NewPool = Pool+1,
        {Idents,NewEqs,NewPId,NewPEq,NewPool}; (true) ->
        Ball end (Eq0 == Eq1),
    Res.
clean(Idents,PerId,PerEq) ->
    Sets = set:domain(PerEq),
    Count = set:count(Sets),
    NewEqs = set:range(Count),
    Fun = fun(Id) ->
        Eq = set:get(PerId,Id),
        Ids = set:get(PerEq,Eq),
        set:get(Count,Ids) end,
    NewPId = set:remap(Idents,Fun)
    NewPEq = set:inverse(PerEq),
    NewPool = set:empty(),
    {Idents,NewEqs,NewPId,NewPEq,NewPool}.
sub(Ball,Ids) ->
    {Idents,Equivs,PerId,PerEq,_} = Ball,
    NewIds = set:intersect(Idents,Ids),
    PIdFun = fun(Id,Eq) ->
        set:member(Ids,Id) end,
    NewPId = set:filter(PerId,PIdFun),
    PEqFun = fun(Eq,Set) ->
        set:intersect(Set,Ids) end,
    NewPEq = set:remap(PerEq,PEqFun),
    clean(NewIds,NewPId,NewPEq).
sup(Ball,Other) ->
    {Idents0,Equivs0,PerId0,PerEq0,Pool0} = Ball,
    {Idents1,Equivs1,PerId1,PerEq1,_} = Ball,
    NewIds = set:union(Idents0,Idents1),
    Count = set:count(Equivs1),
    Size = set:length(Equivs0)+Pool0,
    Fun = fun(Eq) ->
        set:get(Count,Eq)+Size end,
    NewPId1 = set:remap(PerId1,Fun),
    PEqFun = fun(Eq,Ids) ->
        {Fun(Eq),Ids} end,
    NewPEq1 = set:map(PerEq1,PEqFun),
    NewPId = set:union(PerId0,NewPId1),
    NewPEq = set:union(PerEq0,NewPEq1),
    clean(NewIds,NewPId,NewPEq).
empty() ->
    Idents = set:empty(),
    Equivs = set:empty(),
    PerId = set:empty(),
    PerEq = set:empty(),
    {Idents,Equivs,PerId,PerEq,0}.
