-module(set).
-export([flat/1,tree/1,convert/2]).
-export([filter/2,map/2,image/2,compare/2]).
-export([difference/2,intersect/2,union/2,family_union/1]).
-export([any/2,foil_any/3,length/1,singleton/2]).
-export([insert/2,remove/2,member/2,domain/1,range/1]).
-export([ident/1,key/1,val/1,get/2,find/5]).
-export([open/3,open_d/3,open_r/3,]).
-export([closed/3,closed_d/3,closed_r/3]).
-export([clopen/3,clopen_d/3,clopen_r/3]).
-export([opclosed/3,opclosed_d/3,opclosed_r/3]).
flat({tree,List}) ->
    {flat,lists:reverse(flat(List,[]))}.
flat([],Acc) ->
    Acc;
flat(List,Acc) ->
    [H|T] = List,
    Res = fun({leaf,Elem}) ->
        [Elem|Acc]; ({node,Sub}) ->
        flat(Sub,Acc) end (H),
    flat(T,Res).
tree({flat,List}) ->
    {tree,lists:reverse(tree(List))};
tree([]) ->
    [];
tree([H]) ->
    {leaf,H};
tree([H0,H1|T]) ->
    List = [H0,H1|T],
    {L,R} = lists:split(erlang:length(List) div 2,List),
    {node,[tree(L),tree(R)]}.
convert(Tag,[{Tag,List}|T]) ->
    {[{Tag,List}],[{Tag,List}|T]};
convert(flat,[{tree,List}|T]) ->
    {Res,Acc} = convert(flat,T),
    fun([]) ->
        Flat = flat({tree,List}),
        {[Flat],[Flat,{tree,List}|T]}; (_) ->
        {Res,Acc} end (Res);
convert(tree,[{flat,List}|T]) ->
    {Res,Acc} = convert(tree,T),
    fun([]) ->
        Tree = tree({flat,List}),
        {[Tree],[Tree,{flat,List}|T]}; (_) ->
        {Res,Acc} end (Res);
convert(Tag,[H|T]) ->
    {Res,Acc} = convert(Tag,T),
    {Res,[H|Acc]};
convert(_,[]) ->
    {[],[]}.
filter({flat,List},Fun) ->
    {flat,lists:reverse(filter(List,Fun,[]))};
filter(List,Fun) ->
    {[Flat],Acc} = convert(flat,List),
    {[filter(Flat,Fun)],Acc}.
filter([],_,Acc) ->
    Acc;
filter(List,Fun,Acc) ->
    [H|T] = List,
    Res = fun(true) ->
        [H,Acc]; (false) ->
        Acc end (Fun(H)),
    filter(T,Fun,Res).
map({flat,List},Fun) ->
    {flat,sort(map(List,Fun,[]),fun key/1)};
map(List,Fun) ->
    {[Flat],Acc} = convert(flat,List),
    {[map(Flat,Fun)],Acc}.
map([],_,Acc) ->
    Acc;
map(List,Fun,Acc) ->
    [H|T] = List,
    Res = [Fun(H)|Acc],
    map(T,Fun,Res).
sort({flat,[]}) ->
    {flat,[]};
sort({flat,[H]}) ->
    {flat,[H]};
sort({flat,List}) ->
    {L,R} = lists:split(erlang:length(List) div 2,List),
    {flat,union({flat,L},{flat,R})};
sort(List) ->
    {[Flat],Acc} = convert(flat,List),
    {[sort(Flat)],Acc}.
image(Map,Set) ->
    Fun = fun(Elem) ->
        get(Map,Elem) end,
    map(Set,Fun).
compare(L,R) ->
    RFun = fun(true) ->
        1; (false) ->
        0 end,
    LFun = fun(true) ->
        -1; (false) ->
        RFun(L>R) end,
    LFun(L<R).
difference({flat,L},{flat,R}) ->
    {flat,lists:reverse(difference(L,R,[]))};
difference(L,R) ->
    {Lf,La} = convert(flat,L),
    {Rf,Ra} = convert(flat,R),
    {[difference(Lf,Rf)],La,Ra}.
difference([],[],Acc) ->
    Acc;
difference([],R,Acc) ->
    [Rh|Rt] = R,
    difference([],Rt,Acc);
difference(L,[],Acc) ->
    [Lh|Lt] = L,
    difference(Lt,[],[Lh|Acc]);
difference(L,R,Acc) ->
    [Lh|Lt] = L,
    [Rh|Rt] = R,
    {Lr,Rr,Res} = fun(-1) ->
        {Lt,R,[Lh,Acc]}; (0) ->
        {Lt,Rt,Acc}; (1) ->
        {L,Rt,Acc} end (compare(Lh,Rh)),
    difference(Lr,Rr,Res).
intersect({flat,L},{flat,R}) ->
    {flat,lists:reverse(intersect(L,R,[]))};
intersect(L,R) ->
    {Lf,La} = convert(flat,L),
    {Rf,Ra} = convert(flat,R),
    {[intersect(Lf,Rf)],La,Ra}.
intersect([],[],Acc) ->
    Acc;
intersect([],R,Acc) ->
    [Rh|Rt] = R,
    intersect([],Rt,Acc);
intersect(L,[],Acc) ->
    [Lh|Lt] = L,
    intersect(Lt,[],Acc);
intersect(L,R,Acc) ->
    [Lh|Lt] = L,
    [Rh|Rt] = R,
    {Lr,Rr,Res} = fun(-1) ->
        {Lt,R,Acc}; (0) ->
        {Lt,Rt,[Lh|Acc]}; (1) ->
        {L,Rt,Acc} end (compare(Lh,Rh)),
        intersect(Lr,Rr,Res).
union({flat,L},{flat,R}) ->
    {flat,lists:reverse(union(L,R,[]))};
union(L,R) ->
    {Lf,La} = convert(flat,L),
    {Rf,Ra} = convert(flat,R),
    {[union(Lf,Rf)],La,Ra}.
union([],[],Acc) ->
    Acc;
union([],R,Acc) ->
    [Rh|Rt] = R,
    union([],Rt,[Rh|Acc]);
union(L,[],Acc) ->
    [Lh|Lt] = L,
    union(Lt,[],[Lh|Acc]);
union(L,R,Acc) ->
    [Lh|Lt] = L,
    [Rh|Rt] = R,
    {Lr,Rr,Res} = fun(-1) ->
        {Lt,R,[Lh|Acc]}; (0) ->
        {Lt,Rt,[Lh|Acc]}; (1) ->
        {L,Rt,[Rh|Acc]} end (compare(Lh,Rh)),
    union(Lr,Rr,Res).
family_union({flat,List}) ->
    {family_union(List,[]),defer};
family_union(List) ->
    {Flat,Acc} = convert(flat,List),
    {[family_union(Flat)],Acc}.
family_union([],Acc) ->
    Acc;
family_union(List,Acc) ->
    [H|T] = List,
    family_union(T,union(H,Acc)).
any({flat,List},Fun) ->
    try any_r(List,Fun) of false ->
        false catch true ->
        true end;
any(List,Fun) ->
    {Flat,Acc} = convert(flat,List),
    {any(Flat,Fun),Opt}.
any_r([],_) ->
    false;
any_r(List,Fun) ->
    [H,T] = List,
    Fun(H) andalso throw(true),
    any_r(T,Fun).
foil_any({flat,L},{flat,R},Fun) ->
    try foil_any_r(L,R,Fun) of false ->
        false catch true ->
        true end;
foil_any(L,R,Fun) ->
    {Lf,La} = convert(flat,L),
    {Rf,Ra} = convert(flat,R),
    {foil_any(Lf,Rf,Fun),La,Ra}.
foil_any_r(_,[],_) ->
    false;
foil_any_r([],_,_) ->
    false;
foil_any_r(L,R,Fun) ->
    [Lh|Lt] = L,
    [Rh|Rt] = R,
    Lf = fun(Elem) ->
        Fun(Elem,Rh) end,
    Rf = fun(Elem) ->
        Fun(Lh,Elem) end,
    Fun(Lh,Rh) andalso throw(true),
    any(Lt,Lf) andalso throw(true),
    any(Rt,Rf) andalso throw(true),
    foil_any_r(Lt,Rt,Fun).
length({flat,List}) ->
    erlang:length(List);
length(List) ->
    {Flat,Acc} = convert(flat,List),
    {length(Flat),Acc}.
singleton(Elem) ->
    {flat,[Elem]}.
insert(Set,Elem) ->
    Single = singleton(Elem),
    union(Set,Single).
remove(Set,Elem) ->
    Single = singleton(Elem),
    difference(Set,Single).
member(Set,Elem) ->
    Found = closed(Set,Elem,Elem),
    length(Found)>0.
domain({flat,List}) ->
    Fun = fun(Pair) ->
        {L,_} = Pair,
        L end,
    map(List,Fun);
domain(List) ->
    {Flat,Acc} = convert(flat,List),
    {domain(Flat),Acc}.
range({flat,List}) ->
    Fun = fun(Pair) ->
        {_,R} = Pair,
        R end,
    map(List,Fun);
range(List) ->
    {Flat,Acc} = convert(flat,List),
    {range(Flat),Acc}.
ident(Elem) ->
    Elem.
key(Elem) ->
    {Key,_} = Elem,
    Key.
val(Elem) ->
    {_,Val} = Elem,
    Val.
get(Set,Key) ->
    [Res] = closed_r(Set,Key,Key),
    Res.
find({tree,List},L,R,K,V) ->
    Rev = try find(List,L,R,K,V,[]) of Val ->
        Val catch Val ->
        Val end,
    lists:reverse(Rev);
find(List,L,R,K,V) ->
    {Tree,Acc} = convert(tree,List),
    {find(Tree,L,R,K,V),Acc}.
find([],_,_,_,_,Acc) ->
    Acc.
find([{node,Sub}|T],L,R,K,V,Acc) ->
    Depth = find(Sub,L,R,K,V,Acc),
    find(T,L,R,K,V,Depth).
find([{leaf,Elem}|T],L,R,K,V,Acc) ->
    Key = K(Elem),
    Left = L(Key),
    Right = R(Key),
    RFun = fun(true) ->
        throw(Acc); (false) ->
        Val = V(Elem),
        [Val|find(T,L,R,K,V,Acc)] end,
    LFun  = fun(true) ->
        find(T,L,R,K,V,Acc); (false) ->
        RFun(Right) end,
    LFun(Left).
open(Set,L,R) ->
    open(Set,L,R,fun ident/1,fun ident/1).
open_d(Set,L,R) ->
    open(Set,L,R,fun key/1,fun key/1).
open_r(Set,L,R) ->
    open(Set,L,R,fun key/1,fun val/1).
open(Set,L,R,K,V) ->
    LFun = fun(Key) ->
        Key=<L end,
    RFun = fun(Key) ->
        Key>=R end,
    find(Set,LFun,RFun,K,V).
closed(Set,Min,Max) ->
    closed(Set,Min,Max,fun ident/1,fun ident/1).
closed_d(Set,Min,Max) ->
    closed(Set,Min,Max,fun key/1,fun key/1).
closed_r(Set,Min,Max) ->
    closed(Set,Min,Max,fun key/1,fun val/1).
closed(Set,Min,Max,K,V) ->
    LFun = fun(Key) ->
        Key<L end,
    RFun = fun(Key) ->
        Key>R end,
    find(Set,LFun,RFun,K,V).
clopen(Set,Min,Lim) ->
    clopen(Set,Min,Lim,fun ident/1,fun ident/1).
clopen_d(Set,Min,Lim) ->
    clopen(Set,Min,Lim,fun key/1,fun key/1).
clopen_r(Set,Min,Lim) ->
    clopen(Set,Min,Lim,fun key/1,fun val/1).
clopen(Set,Min,Lim,K,V) ->
    LFun = fun(Key) ->
        Key<L end,
    RFun = fun(Key) ->
        Key>=R end,
    find(Set,LFun,RFun,K,V).
opclosed(Set,Lim,Max) ->
    opclosed(Set,Lim,Max,fun ident/1,fun ident/1).
opclosed_d(Set,Lim,Max) ->
    opclosed(Set,Lim,Max,fun key/1,fun key/1).
opclosed_r(Set,Lim,Max) ->
    opclosed(Set,Lim,Max,fun key/1,fun val/1).
opclosed(Set,Lim,Max,K,V) ->
    LFun = fun(Key) ->
        Key=<L end,
    RFun = fun(Key) ->
        Key>R end,
    find(Set,LFun,RFun,K,V).
