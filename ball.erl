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
    myset:filter(Idents,Fun).
class(Ball,Ids) ->
    {_,_,PerId,_,_} = Ball,
    myset:image(PerId,Ids).
ident(Ball,Eqs) ->
    {_,_,_,PerEq,_} = Ball,
    Fam = myset:image(PerEq,Eqs),
    myset:family_union(Fam).
get(Ball,Class) ->
    {_,Equivs,_,PerEq,_} = Ball,
    Ids = mymap:get(PerEq,Class),
    Fun = fun(Eq) ->
        Set = mymap:get(PerEq,Eq),
        Fun = fun(L,R) ->
            PreL = lists:droplast(L),
            PreR = lists:droplast(R),
            (L == R) or (PreL == R) or (L == PreR) end,
        myset:foil_any(Ids,Set,Fun) end,
    myset:filter(Equivs,Fun).
put(Ball,Ident) ->
    {Idents,Equivs,PerId,PerEq,Pool} = Ball,
    Equiv = myset:length(Equivs)+Pool,
    NewIds = myset:insert(Idents,Ident),
    NewEqs = myset:insert(Equivs,Equiv),
    NewPId = mymap:insert(PerId,Ident,Equiv),
    Ids = myset:single(Ident),
    NewPEq = myset:insert(PerEq,Equiv,Ids),
    {NewIds,NewEqs,NewPId,NewPEq,Pool}.
put(Ball,Ident,Other) ->
    {Idents,Equivs,PerId,PerEq,Pool} = Ball,
    Equiv = mymap:get(PerId,Other),
    NewIds = myset:insert(Idents,Ident),
    NewPId = mymap:insert(PerId,Ident,Equiv),
    Ids = mymap:get(PerEq,Equiv),
    Set = myset:insert(Ids,Ident),
    NewPEq = mymap:replace(PerEq,Equiv,Set),
    {NewIds,Equivs,NewPId,NewPEq,Pool}.
same(Ball,Ident,Other) ->
    {Idents,Equivs,PerId,PerEq,Pool} = Ball,
    Eq0 = mymap:get(PerId,Ident),
    Eq1 = mymap:get(PerId,Other),
    Res = fun(false) ->
        Ids = mymap:get(PerEq,Eq0),
        Oth = mymap:get(PerEq,Eq1),
        Set = myset:union(Ids,Oth),
        New = mymap:replace(PerEq,Eq0,Set),
        NewPEq = mymap:remove(New,Eq1),
        NewEqs = myset:remove(Equivs,Eq1),
        Fun = fun(Id,Eq) ->
            Res = fun(true) ->
                Eq0; (false) ->
                Eq end (myset:member(Oth,Id)),
            Res end,
        NewPId = mymap:remap(PerId,Fun),
        NewPool = Pool+1,
        {Idents,NewEqs,NewPId,NewPEq,NewPool}; (true) ->
        Ball end (Eq0 == Eq1),
    Res.
clean(Idents,PerId,PerEq) ->
    Sets = myset:domain(PerEq),
    Count = mymap:count(Sets),
    NewEqs = myset:range(Count),
    Fun = fun(Id) ->
        Eq = mymap:get(PerId,Id),
        Ids = mymap:get(PerEq,Eq),
        mymap:get(Count,Ids) end,
    NewPId = mymap:remap(Idents,Fun)
    NewPEq = mymap:inverse(PerEq),
    NewPool = myset:empty(),
    {Idents,NewEqs,NewPId,NewPEq,NewPool}.
sub(Ball,Ids) ->
    {Idents,Equivs,PerId,PerEq,_} = Ball,
    NewIds = myset:intersect(Idents,Ids),
    PIdFun = fun(Id,Eq) ->
        myset:member(Ids,Id) end,
    NewPId = mymap:filter(PerId,PIdFun),
    PEqFun = fun(Eq,Set) ->
        myset:intersect(Set,Ids) end,
    NewPEq = mymap:remap(PerEq,PEqFun),
    clean(NewIds,NewPId,NewPEq).
sup(Ball,Other) ->
    {Idents0,Equivs0,PerId0,PerEq0,Pool0} = Ball,
    {Idents1,Equivs1,PerId1,PerEq1,_} = Ball,
    NewIds = myset:union(Idents0,Idents1),
    Count = mymap:count(Equivs1),
    Size = myset:length(Equivs0)+Pool0,
    Fun = fun(Eq) ->
        mymap:get(Count,Eq)+Size end,
    NewPId1 = mymap:remap(PerId1,Fun),
    PEqFun = fun(Eq,Ids) ->
        {Fun(Eq),Ids} end,
    NewPEq1 = mymap:map(PerEq1,PEqFun),
    NewPId = mymap:union(PerId0,NewPId1),
    NewPEq = mymap:union(PerEq0,NewPEq1),
    clean(NewIds,NewPId,NewPEq).
empty() ->
    Idents = myset:empty(),
    Equivs = myset:empty(),
    PerId = mymap:empty(),
    PerEq = mymap:empty(),
    {Idents,Equivs,PerId,PerEq,0}.
