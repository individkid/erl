-module(set).
-export([convert/2,convert/6,select/2,table/2]).
-export([flat/1,tree/1,compare/2,ident/1,key/1,val/1]).
-export([filter/2,map/2,map/3,map/4,tabulate/2,tabulate/3,tabulate/4]).
-export([any/2,any2/3,find/5,combine/5,combine/6]).
-export([difference/2,symmetric/2]).
-export([intersect/2,intersectk/2,intersectf/2]).
-export([union/2,unionk/2,unionf/1]).
-export([open/3,openk/3,openv/3,]).
-export([closed/3,closedk/3,closedv/3]).
-export([clopen/3,clopenk/3,clopenv/3]).
-export([opclosed/3,opclosedk/3,opclosedv/3]).
-export([uniquefy/1,sort/1,image/2,inverse/1,reverse/2]).
-export([domain/1,range/1,insert/2,remove/2]).
-export([length/1,singleton/1,member/2,get/2,count/1,counti/1]).
bind0(Arg0,Fun) ->
    fun() -> Fun(Arg0) end.
bind0(Arg0,Arg1,Fun) ->
    fun() -> Fun(Arg0,Arg1) end.
bind0(Arg0,Arg1,Arg2,Fun) ->
    fun() -> Fun(Arg0,Arg1,Arg2) end.
bind1(Arg0,Fun) ->
    fun(Arg1) -> Fun(Arg0,Arg1) end.
bind1(Arg0,Arg1,Fun) ->
    fun(Arg2) -> Fun(Arg0,Arg1,Arg2) end.
bind1(Arg0,Arg1,Arg2,Fun) ->
    fun(Arg3) -> Fun(Arg0,Arg1,Arg2,Arg3) end.
bind2(Arg0,Fun) ->
    fun(Arg1,Arg2) -> Fun(Arg0,Arg1,Arg2) end.
bind2(Arg0,Arg1,Fun) ->
    fun(Arg2,Arg3) -> Fun(Arg0,Arg1,Arg2,Arg3) end.
bind2(Arg0,Arg1,Arg2,Fun) ->
    fun(Arg3,Arg4) -> Fun(Arg0,Arg1,Arg2,Arg3,Arg4) end.
convert([H|T],{Tag,Tab}) ->
    list:reverse(
        convert_r([H|T],{Tag,Tab},[{Tag,Tab}],false,
            fun convert/2,fun convert/3,fun convert/4,fun convert/5,fun convert/6));
convert(flat,{tree,Tab}) ->
    flat({tree,Tab});
convert(tree,{flat,Tab}) ->
    tree({flat,Tab});
convert(_,{_,_}) ->
    throw([]);
convert(_,{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
convert_r([H|T],{Tag0,Tab0},Acc,false,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun1(H,{Tag0,Tab0}),
        convert_r(T,{Tag0,Tab0},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([H|T],{Tag0,Tab0},Acc,true,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun1(H,{Tag0,Tab0}),
        convert_r(T,{Tag0,Tab0},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([],_,Acc,_,_,_,_,_) ->
    Acc.
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},Acc,false,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun2(H,{Tag0,Tag0},{Tag1,Tab1}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},Acc,true,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun2(H,{Tag0,Tag0},{Tag1,Tab1}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([],_,_,Acc,_,_,_,_,_) ->
    Acc.
convert([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},Acc,false,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},Acc,true,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([],_,_,_,Acc,_,_,_,_,_) ->
    Acc.
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},Acc,false,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4) catch _ ->
        Conv = Fun4(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},Acc,true,Fun1,Fun2,Fun3,Fun4) ->
    try select(H,Acc) of {Tag,Tab} ->
        throw([]) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4) end;
convert_r([],_,_,_,_,Acc,_,_,_,_,_) ->
    Acc.
select(Tag,[{Tag,Tab}|T]) ->
    {Tag,Tab};
select(To,[{Tag,Tab}|T]) ->
    select(To,T).
table(To,From) ->
    {Tag,Tab} = select(To,From),
    Tab.
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
    {tree,[]};
tree({flat,Flat}) ->
    {tree,lists:reverse(tree_r(Flat))};
tree_r([H]) ->
    {leaf,H};
tree_r([H0,H1|T]) ->
    Len = erlang:length([H0,H1|T]) div 2,
    {L,R} = lists:split(Len,[H0,H1|T]),
    {node,[tree(L),tree(R)]}.
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
filter({flat,Flat},Fun) ->
    {flat,lists:reverse(filter_r(Flat,Fun,[]))}.
filter_r([],_,Acc) ->
    Acc;
filter_r([H|T],Fun,Acc) ->
    Res = fun(true) ->
        [H,Acc]; (false) ->
        Acc end(Fun(H)),
    filter(T,Fun,Res).
map({flat,Flat},Fun) ->
    {flat,sort(map_r(Flat,Fun,[]))}.
map_r([],_,Acc) ->
    Acc;
map_r([H|T],Fun,Acc) ->
    Res = [Fun(H)|Acc],
    map_r(T,Fun,Res).
map({flat,Flat0},{flat,Flat1},Fun) ->
    {flat,sort(map_r(Flat0,Flat1,Fun,[]))}.
map_r([],[],_,Acc) ->
    Acc;
map_r([H0|T0],[H1|T1],Fun,Acc) ->
    Res = [Fun(H0,H1)|Acc],
    map_r(T0,T1,Fun,Res).
map({flat,Flat0},{flat,Flat1},{flat,Flat2},Fun) ->
    {flat,sort(map_r(Flat0,Flat1,Flat2,Fun,[]))}.
map_r([],[],[],_,Acc) ->
    Acc;
map_r([H0|T0],[H1|T1],[H2|T2],Fun,Acc) ->
    Res = [Fun(H0,H1,H2)|Acc],
    map_r(T0,T1,T2,Fun,Res).
map({flat,Flat0},{flat,Flat1},{flat,Flat2},{flat,Flat3},Fun) ->
    {flat,sort(map_r(Flat0,Flat1,Flat2,Flat3,Fun,[]))}.
map_r([],[],[],[],_,Acc) ->
    Acc;
map_r([H0|T0],[H1|T1],[H2|T2],[H3|T3],Fun,Acc) ->
    Res = [Fun(H0,H1,H2,H3)|Acc],
    map_r(T0,T1,T2,T3,Fun,Res).
tabulate({flat,Flat},Giv) ->
    Fun = fun(Elem) ->
        {Elem,Giv(Elem)} end,
    {flat,map({flat,Flat},Fun)}.
tabulate({flat,Lf},{flat,Rf},Giv) ->
    Fun = fun(El) ->
        Fun = fun(Er) ->
            Giv(El,Er) end,
        tabulate(Rf,Fun) end,
    {flat,map(Lf,Fun)}.
tabulate({flat,Lf},{flat,Mf},{flat,Rf},Giv) ->
    Fun = fun(El) ->
        Fun = fun(Em,Er) ->
            Giv(El,Em,Er) end,
        tabulate(Mf,Rf,Fun) end,
    {flat,map(Lf,Fun)}.
any({flat,Flat},Fun) ->
    try any_r(Flat,Fun) of false ->
        false catch true ->
        true end.
any_r([],_) ->
    false;
any_r([H|T],Fun) ->
    Fun(H) andalso throw(true),
    any_r(T,Fun).
any2({flat,L},{flat,R},Fun) ->
    try any2_r(L,R,Fun) of false ->
        false catch true ->
        true end.
any2_r(_,[],_) ->
    false;
any2_r([],_,_) ->
    false;
any2_r([Lh|Lt],[Rh|Rt],Fun) ->
    Lf = fun(Elem) ->
        Fun(Elem,Rh) end,
    Rf = fun(Elem) ->
        Fun(Lh,Elem) end,
    Fun(Lh,Rh) andalso throw(true),
    any(Lt,Lf) andalso throw(true),
    any(Rt,Rf) andalso throw(true),
    any2_r(Lt,Rt,Fun).
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
find({tree,Tree},L,R,K,V) ->
    Rev = try find_r(Tree,L,R,K,V,[]) of Val ->
        Val catch Val ->
        Val end,
    {flat,lists:reverse(Rev)}.
find_r([],_,_,_,_,Acc) ->
    Acc;
find_r([{node,Sub}|T],L,R,K,V,Acc) ->
    Depth = find(Sub,L,R,K,V,Acc),
    find(T,L,R,K,V,Depth);
find_r([{leaf,Elem}|T],L,R,K,V,Acc) ->
    Key = K(Elem),
    Left = L(Key),
    Right = R(Key),
    Fun = fun(true) ->
        throw(Acc); (false) ->
        Val = V(Elem),
        [Val|find(T,L,R,K,V,Acc)] end,
    fun(true) ->
        find(T,L,R,K,V,Acc); (false) ->
        Fun(Right) end(Left).
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
sort({flat,[]}) ->
    {flat,[]};
sort({flat,[H]}) ->
    {flat,[H]};
sort({flat,Flat}) ->
    {L,R} = lists:split(erlang:length(Flat) div 2,Flat),
    {flat,union({flat,L},{flat,R})}.
sortk({flat,[]}) ->
    {flat,[]};
sortk({flat,[H]}) ->
    {flat,[H]};
sortk({flat,Flat}) ->
    {L,R} = lists:split(erlang:length(Flat) div 2,Flat),
    {flat,unionk({flat,L},{flat,R})}.
image({flat,Set_f},{tree,Map_t}) ->
    Fun = fun(Elem) ->
        closedv({tree,Map_t},Elem,Elem) end,
    unionf(map({flat,Set_f},Fun)).
inverse({flat,Flat}) ->
    Fun = fun({Key,Val}) ->
        {Val,Key} end,
    map({flat,Flat},Fun).
reverse({flat,Set_f},{flat,Map_f}) ->
    {flat,Inv_f} = inverse({flat,Map_f}),
    {tree,Inv_t} = convert(tree,{flat,Inv_f}),
    {flat,Res} = image({flat,Flat},{tree,Inv_t}),
    {flat,Res}.
domain({flat,Flat}) ->
    map({flat,Flat},fun key/1).
range({flat,Flat}) ->
    uniquefy(map({flat,Flat},fun val/1)).
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
count({flat,Flat}) ->
    {flat,count_r({flat,Flat},[],1)}.
count_r([],Acc,_) ->
    Acc;
count_r({flat,Flat},Acc,Num) ->
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
