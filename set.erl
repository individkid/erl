-module(set).

-export([access/2,access/3]).
-export([flat/1,tree/1,deept/1,deepf/1,list/1]).
-export([compare/2,ident/1,key/1,val/1]).
-export([combine/5,combine/6,find/5]).
-export([difference/2,symmetric/2]).
-export([intersect/2,intersectk/2,intersectf/2]).
-export([union/2,unionk/2,unionf/1]).
-export([open/3,openk/3,openv/3,]).
-export([closed/3,closedk/3,closedv/3]).
-export([clopen/3,clopenk/3,clopenv/3]).
-export([opclosed/3,opclosedk/3,opclosedv/3]).
-export([uniquefy/1,uniquefyk/1,uniquefyv/1]).
-export([sort/1,image/2,inverse/1,reverse/2]).
-export([domain/1,range/1,insert/2,remove/2]).
-export([length/1,singleton/1,member/2,get/2]).
-export([hole/1,holes/2,count/1,counti/1]).
-export([sets/1,sets/2,lists/1,lists/2]).

access({_,Tab_t},Arg) ->
    set:get({tree,Tab_t},Arg).
access({_,Tab_t},Arg0,Arg1) ->
    set:get({tree,set:get({tree,Tab_t},Arg0)},Arg1).

flat({tree,Tree}) ->
    {flat,lists:reverse(flat_r(Tree,[]))}.
flat_r([],Acc) ->
    Acc;
flat_r([H|T],Acc) ->
    Res = fun({leaf,Elem}) ->
        [Elem|Acc]; ({node,Sub}) ->
        flat(Sub,Acc) end(H),
    flat(T,Res).
tree({flat,[]}) ->
    {tree,{[],[]}};
tree({flat,Flat}) ->
    {Leaves,Nodes} = tree_r(Flat),
    {tree,{Leaves,Nodes}};
tree_r([H]) ->
    {[H],[]};
tree_r([H0,H1]) ->
    {[H0,H1],[]};
tree_r([H0,H1,H2|T]) ->
    Len = erlang:length([H0,H1,H2|T]) div 2,
    {L,[M|R]} = lists:split(Len,[H0,H1,H2|T]),
    {[M],[tree_r(L),tree_r(R)]}.

list({flat,List}) ->
    List.

compare(L,R) ->
    Fun = fun(true) ->
        1; (false) ->
        0 end,
    fun(true) ->
        -1; (false) ->
        Fun(L>R) end(L<R).
ident(Elem) ->
    Elem.
key(Elem) ->
    {Key,_} = Elem,
    Key.
val(Elem) ->
    {_,Val} = Elem,
    Val.

combine({flat,L},{flat,R},Lf,Mf,Rf) ->
    Fun = fun(L,R) -> compare(L,R) end,
    lists:reverse(combine_r(L,R,[],Lf,Mf,Rf,Fun)).
combine({flat,L},{flat,R},Lf,Mf,Rf,Comp) ->
    lists:reverse(combine_r(L,R,[],Lf,Mf,Rf,Comp)).
combine_r([],[],Acc,_,_,_,_) ->
    Acc;
combine_r([],R,Acc,_,_,Rf,_) ->
    [Rh|Rt] = R,
    combine_r([],Rt,Rf(Rh,Acc),_,_,Rf,_);
combine_r(L,[],Acc,Lf,_,_,_) ->
    [Lh|Lt] = L,
    combine_r(Lt,[],Lf(Lh,Acc),Lf,_,_,_);
combine_r([Lh|Lt],[Rh|Rt],Acc,Lf,Mf,Rf,Comp) ->
    {Lr,Rr,Res} = fun(-1) ->
        {Lt,R,Lf(Lh,Acc)}; (0) ->
        {Lt,Rt,Mf(Lh,Acc)}; (1) ->
        {L,Rt,Rf(Rh,Acc)} end(Comp(Lh,Rh)),
    combine_r(Lr,Rr,Res,Lf,Mf,Rf,Comp).
find({tree,{Leaves,Nodes}},L,R,K,V) ->
    {flat,lists:reverse(find_r(Leaves,Nodes,L,R,K,V,[]))}.
find_r([Leaf|Leaves],[{Leaves0,Nodes0},{Leaves1,Nodes1}|Nodes],L,R,K,V,Acc) ->
    Key = K(Leaf),
    Left = L(Key),
    Right = R(Key),
    fun(true,true) ->
        throw([]);
    (true,false) ->
        find_r(Leaves,[{Leaves1,Nodes1}|Nodes],L,R,K,V,Acc);
    (false,true) ->
        find_r(Leaves0,Nodes0,L,R,K,V,Acc);
    (false,false) ->
        find_r(Leaves,[{Leaves1,Nodes1}|Nodes],L,R,K,V,V(Leaf)|find_r(Leaves0,Nodes0,L,R,K,V,Acc)]) end
    (Left,Right);
find_r([Leaf|Leaves],[],L,R,K,V,Acc) ->
    Key = K(Leaf),
    Left = L(Key),
    Right = R(Key),
    fun(true,true) ->
        throw([]);
    (true,false) ->
        find_r(Leaves,[],L,R,K,V,Acc);
    (false,true) ->
        Acc;
    (false,false) ->
        find_r(Leaves,[],L,R,K,V,V(Leaf)|Acc) end
    (Left,Right);
find_r([],[{Leaves,Nodes}],L,R,K,V,Acc) ->
    find_r(Leaves,Nodes,L,R,K,V,Acc);
find_r([],[],_,_,_,_,Acc) ->
    Acc.

difference({flat,L},{flat,R}) ->
    Lf = fun(H,T) -> [H|T] end,
    Mf = fun(H,T) -> T end,
    Rf = fun(H,T) -> T end,
    {flat,combine({flat,L},{flat,R},Lf,Mf,Rf)}.
symmetric({flat,L},{flat,R}) ->
    Lf = fun(H,T) -> [H|T] end,
    Mf = fun(H,T) -> T end,
    Rf = fun(H,T) -> [H|T] end,
    {flat,combine({flat,L},{flat,R},Lf,Mf,Rf)}.
intersect({flat,L},{flat,R}) ->
    Lf = fun(H,T) -> T end,
    Mf = fun(H,T) -> [H|T] end,
    Rf = fun(H,T) -> T end,
    {flat,combine({flat,L},{flat,R},Lf,Mf,Rf)}.
intersectk({flat,L},{flat,R}) ->
    Lf = fun(H,T) -> T end,
    Mf = fun(H,T) -> [H|T] end,
    Rf = fun(H,T) -> T end,
    Comp = fun(L,R) ->
        {Lk,_} = L,
        {Rk,_} = R,
        compare(Lk,Rk) end,
    {flat,combine({flat,L},{flat,R},Lf,Mf,Rf,Comp)}.
intersectf({flat,Flat}) ->
    {flat,intersectf_r(Flat,[])}.
intersectf_r([],Acc) ->
    Acc;
intersectf_r(Flat,Acc) ->
    [H|T] = Flat,
    intersectf_r(T,intersect(H,Acc)).
union({flat,L},{flat,R}) ->
    Lf = fun(H,T) -> [H|T] end,
    Mf = fun(H,T) -> [H|T] end,
    Rf = fun(H,T) -> [H|T] end,
    {flat,combine({flat,L},{flat,R},Lf,Mf,Rf)}.
unionk({flat,L},{flat,R}) ->
    Lf = fun(H,T) -> [H|T] end,
    Mf = fun(H,T) -> [H|T] end,
    Rf = fun(H,T) -> [H|T] end,
    Comp = fun(L,R) ->
        {Lk,_} = L,
        {Rk,_} = R,
        compare(Lk,Rk) end,
    {flat,combine({flat,L},{flat,R},Lf,Mf,Rf,Comp)}.
unionf({flat,Flat}) ->
    {flat,unionf_r(Flat,[])}.
unionf_r([],Acc) ->
    Acc;
unionf_r(Flat,Acc) ->
    [H|T] = Flat,
    unionf_r(T,union(H,Acc)).

open({tree,Tree},L,R) ->
    open_r({tree,Tree},L,R,fun ident/1,fun ident/1).
openk({tree,Tree},L,R) ->
    open_r({tree,Tree},L,R,fun key/1,fun key/1).
openv({tree,Tree},L,R) ->
    open_r({tree,Tree},L,R,fun key/1,fun val/1).
open_r({tree,Tree},L,R,K,V) ->
    LFun = fun(Key) ->
        Key=<L end,
    RFun = fun(Key) ->
        Key>=R end,
    find({tree,Tree},LFun,RFun,K,V).
closed({tree,Tree},Min,Max) ->
    closed_r({tree,Tree},Min,Max,fun ident/1,fun ident/1).
closedk({tree,Tree},Min,Max) ->
    closed_r({tree,Tree},Min,Max,fun key/1,fun key/1).
closedv({tree,Tree},Min,Max) ->
    closed_r({tree,Tree},Min,Max,fun key/1,fun val/1).
closed_r({tree,Tree},Min,Max,K,V) ->
    LFun = fun(Key) ->
        Key<Min end,
    RFun = fun(Key) ->
        Key>Max end,
    find({tree,Tree},LFun,RFun,K,V).
clopen({tree,Tree},Min,Lim) ->
    clopen_r({tree,Tree},Min,Lim,fun ident/1,fun ident/1).
clopenk({tree,Tree},Min,Lim) ->
    clopen_r({tree,Tree},Min,Lim,fun key/1,fun key/1).
clopenv({tree,Tree},Min,Lim) ->
    clopen_r({tree,Tree},Min,Lim,fun key/1,fun val/1).
clopen({tree,Tree},Min,Lim,K,V) ->
    LFun = fun(Key) ->
        Key<Min end,
    RFun = fun(Key) ->
        Key>=Lim end,
    find({tree,Tree},LFun,RFun,K,V).
opclosed({tree,Tree},Lim,Max) ->
    opclosed_r({tree,Tree},Lim,Max,fun ident/1,fun ident/1).
opclosedk({tree,Tree},Lim,Max) ->
    opclosed_r({tree,Tree},Lim,Max,fun key/1,fun key/1).
opclosedv({tree,Tree},Lim,Max) ->
    opclosed_r({tree,Tree},Lim,Max,fun key/1,fun val/1).
opclosed({tree,Tree},Lim,Max,K,V) ->
    LFun = fun(Key) ->
        Key=<Lim end,
    RFun = fun(Key) ->
        Key>Max end,
    find({tree,Tree},LFun,RFun,K,V).
uniquefy({flat,Flat}) ->
    {flat,lists:reverse(uniquefy_r(Flat,[]))}.
uniquefy_r([],[]) ->
    [];
uniquefy_r([H|T],Acc) ->
    uniquefy_r(T,[H|Acc],H).
uniquefy_r([],Acc,_) ->
    Acc;
uniquefy_r([H|T],Acc,H) ->
    uniquefy_r(T,Acc,H);
uniquefy_r([H|T],Acc,_) ->
    uniquefy_R(T,[H|Acc],H).
uniquefyk({flat,Flat}) ->
    {flat,lists:reverse(uniquefyk_R(Flat,[]))}.
uniquefyk_r([],[]) ->
    [];
uniquefyk_r([{K,V}|T],Acc) ->
    uniquefy_k(T,[{K,V}|Acc],K).
uniquefyk_r([],Acc,_) ->
    Acc;
uniquefyk_r([{K,V}|T],Acc,K) ->
    uniquefyk_R(T,Acc,K);
uniquefyk_r([{K,V}|T],Acc,_) ->
    uniquefyk_r(T,[{K,V}|Acc],K).
uniquefyv({flat,Flat}) ->
    {flat,lists:reverse(uniquefyv_r(Flat,[]))}.
uniquefyv_r([],[]) ->
    [];
uniquefyv_r([{K,V}|T],Acc) ->
    uniquefyv_r(T,[{K,V}|Acc],V).
uniquefyv_r([],Acc,_) ->
    Acc;
uniquefyv_r([{K,V}|T],Acc,V) ->
    uniquefyv_r(T,Acc,V);
uniquefyv_r([{K,V}|T],Acc,_) ->
    uniquefyv_r(T,[{K,V}|Acc],V).

sort([]) ->
    {flat,[]};
sort([H]) ->
    {flat,[H]};
sort([H|T]) ->
    {L,R} = lists:split(erlang:length([H|T]) div 2,[H|T]),
    {flat,union({flat,L},{flat,R})};
sort(Elem) ->
    Elem.

image({flat,Set_f},{tree,Map_t}) ->
    Fun = fun(Elem) ->
        closedv({tree,Map_t},Elem,Elem) end,
    unionf({flat,map(Set_f,Fun)}).
inverse({flat,Flat}) ->
    Fun = fun({Key,Val}) ->
        {Val,Key} end,
    sort(lists:map(Fun,Flat)).
reverse({flat,Set_f},{flat,Map_f}) ->
    {flat,Inv_f} = inverse({flat,Map_f}),
    {tree,Inv_t} = tree({flat,Inv_f}),
    {flat,Res} = image({flat,Flat},{tree,Inv_t}),
    {flat,Res}.
domain({flat,Flat}) ->
    {flat,lists:map(fun key/1,Flat)}.
range({flat,Flat}) ->
    uniquefy(sort(lists:map(fun val/1,Flat))).
insert({flat,Flat},Elem) ->
    union({flat,Flat},{flat,[Elem]}).
remove({flat,Flat},Elem) ->
    difference({flat,Flat},{flat,[Elem]}).
length({flat,Flat}) ->
    erlang:length(Flat).
singleton(Elem) ->
    {flat,[Elem]}.
member(Set,Elem) ->
    {flat,Found} = closed(Set,Elem,Elem),
    (length(Found) > 0).
get(Map,Key) ->
    {flat,Found} = closedv(Map,Key,Key),
    fun(true) ->
        [Res] = Found,
        Res; (false) ->
        throw([]) end(length(Found).

hole({flat,Flat}) ->
    hole_r(Flat,0).
hole_r([],Elem) ->
    Elem;
hole_r([H|T],H) ->
    hole_r(T,H+1);
hole_r([H|T],Elem) ->
    Elem.
holes({flat,Flat},Length) ->
    holes_r(Flat,Length,0,[]).
holes_r(_,0,_,Acc) ->
    Acc;
holes_r([],Length,Elem,Acc) ->
    holes_r([],Length-1,Elem+1,[Elem|Acc]);
holes_r([H|T],Length,H,Acc) ->
    holes_r(T,Length,H+1,Acc);
holes_r(List,Length,Elem,Acc) ->
    holes_r(List,Length-1,Elem+1,[Elem|Acc]).
count({flat,Flat}) ->
    {flat,count_r(Flat,[],1)}.
count_r([],Acc,_) ->
    Acc;
count_r(Flat,Acc,Num) ->
    Elem0 = choose({flat,Flat}),
    Single0 = singleton(Elem),
    Diff = difference({flat,Flat},Single),
    Elem1 = {Elem0,Num},
    Single1 = singleton(Elem1),
    Map = union_k(Single1,Acc),
    Succ = Num+1,
    count_r(Diff,Map,Succ).
counti({flat,Flat}) ->
    inverse(count({flat,Flat})).

sets({flat,Flat}) ->
    Fun = fun(List) ->
        {flat,List} end,
    Lists = sets_r(Flat),
    Sets = lists:map(Fun,Lists),
    {flat,Sets}.
sets_r([]) ->
    [];
sets_r([H|T]) ->
    Fun = fun(List) ->
        [H|List] end,
    Lists = sets_r(T),
    Larger = lists:map(Fun,Lists),
    Larger++Lists.
sets({flat,Flat},Length) ->
    Fun = fun(List) ->
        {flat,List} end,
    Number = erlang:length(Flat),
    Lists = fun() when Number < Length ->
        [];
    () ->
        sets_r(Flat,erlang:length(Flat),Length) end
    (),
    Sets = lists:map(Fun,Lists),
    {flat,Sets}.
sets_r(_,_,0) ->
    [];
sets_r(List,Len,Length) when Len == Length ->
    List;
sets_r([H|T],Len,Length) when Len > Length ->
    Fun = fun(List) ->
        [H|List] end,
    After = sets_r(T,Len-1,Length),
    Smaller = sets_r(T,Len-1,Length-1),
    Lists = lists:map(Fun,Smaller),
    Lists++After.
sets({flat,Flat},Length,Limit) ->
    .
lists({flat,Flat}) -> %% list of permutations
    sort(lists_r(Flat)).
lists_r([]) -> %% returns list of permutations of given list
    [];
lists_r([H|T]) ->
    Fun = fun(Perm) ->
        lists_r(H,Perm) end,
    lists:append(lists:map(Fun,lists_r(T))).
lists_r(Elem,List) -> %% returns list of list with element inserted in each place
    Fun = fun({L,R}) ->
        L++[Elem|R] end,
    lists:map(Fun,lists_r([],List,[])).
lists_r(L,[],Acc) -> %% returns list of partitions of given list
    [{L,[]}|Acc];
lists_r(L,[H|R],Acc) ->
    lists_r([H|L],R,[{L,[H|R]}|Acc]).
lists({flat,Flat},Length) ->
    Fun = fun({flat,Set}) ->
        {flat,Lists} = lists({flat,Set}),
        Lists end,
    {flat,Sets} = sets({flat,Flat},Length),
    set:sort({flat,lists:append(lists:map(Fun,Sets))}).
