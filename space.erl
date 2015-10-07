-module(space).
%% accessor functions
-export([convert/2,convert/3,convert/4,convert/5,convert/6]).
-export([select/2,table/2]).
-export([side/3,side/4]).
-export([dual/3,dual/4]).
-export([half/3,half/4]).
-export([duali/3,halfi/3]).
-export([boundaries/1,regions/1,sides/1]).
-export([length/1,linear/1,linear/2,dimension/2]).
-export([polyant/3,opposite/5,neighbor/5]).
-export([pencil/5,corner/6]).
-export([attached/5,attached/7,shell/5,shell/7]).
-export([connected/5,connected/6]).
%% functions for spaces in same universe
-export([subspace/4,section/5,subsection/9]).
-export([supersection/8,supersecton/13]).
-export([canonical/4,orderings/5,superspace/6]).
%% functions to convert to and from vectors
-export([sample/4,classify/4]).
convert([H|T],{Tag,Tab}) ->
    list:reverse(
        set:convert([H|T],{Tag,Tab},[{Tag,Tab}],false,
            fun convert/2,fun convert/3,fun convert/4,fun convert/5,fun convert/6));
convert(side_t,{side_f,Tab_f}) ->
    side_t({side_f,Tab_f});
convert(dual_t,{dual_f,Tab_f}) ->
    dual_t({dual_f,Tab_f});
convert(half_t,{dual_f,Tab_f}) ->
    half_t({dual_f,Tab_f});
convert(duali_t,{duali_f,Tab_f}) ->
    duali_t({duali_f,Tab_f});
convert(halfi_t,{halfi_f,Tab_f}) ->
    halfi_t({halfi_f,Tab_f});
convert(boundaries_f,{side_f,Tab_f}) ->
    boundaries_f({side_f,Tab_f});
convert(boundaries_f,{dual_f,Tab_f}) ->
    boundaries_f({dual_f,Tab_f});
convert(boundaries_f,{half_f,Tab_f}) ->
    boundaries_f({half_f,Tab_f});
convert(boundaries_t,{boundaries_f,Tab_f}) ->
    boundaries_t({boundaries_f,Tab_f});
convert(regions_f,{side_f,Tab_f}) ->
    regions_f({side_f,Tab_f});
convert(regions_f,{dual_f,Tab_f}) ->
    regions_f({dual_f,Tab_f});
convert(regions_f,{half_f,Tab_f}) ->
    regions_f({half_f,Tab_f});
convert(regions_t,{regions_f,Tab_f}) ->
    regions_t({regions_f,Tab_f});
convert(sides_f,{side_f,Tab_f}) ->
    sides_f({side_f,Tab_f});
convert(sides_f,{dual_f,Tab_f}) ->
    sides_f({dual_f,Tab_f});
convert(sides_f,{half_f,Tab_f}) ->
    sides_f({half_f,Tab_f});
convert(sides_t,{sides_f,Tab_f}) ->
    sides_t({sides_f,Tab_f});
convert(length,{boundaries_f,Tab_f}) ->
    length({boundaries_f,Tab_f});
convert(linear,{regions_f,Tab_f}) ->
    linear({regions_f,Tab_f});
convert(attached_t,{attached_f,Tab_f}) ->
    attached_t({attached_f,Tab_f});
convert(shell_t,{shell_f,Tab_f}) ->
    shell_t({shell_f,Tab_f});
convert(attachedi_f,{attached_f,Tab_f}) ->
    {attachedi_f,attachedi_f({attached_f,Tab_f})};
convert(attachedi_t,{attachedi_f,Tab_f}) ->
    {attachedi_t,attachedi_t({attachedi_f,Tab_f})};
convert(shelli_f,{shell_f,Tab_f}) ->
    {shelli_f,shelli_f({shell_f,Tab_f})};
convert(shelli_t,{shelli_f,Tab_f}) ->
    {shelli_t,shelli_t({shelli_f,Tab_f})};
convert(_,{_,_}) ->
    throw([]).
convert(duali_f,{dual_t,Tab_t},{sides_f,Sides}) ->
    duali_f({dual_t,Tab_t},{sides_f,Sides});
convert(halfi_f,{half_t,Tab_t},{sides_f,Sides}) ->
    halfi_f({half_t,Tab_t},{sides_f,Sides});
convert(_,{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_}) ->
    throw([]).
convert(side_f,{dual_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    side_f({dual_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(side_f,{half_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    side_f({half_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(dual_f,{side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    dual_f({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(half_f,{side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    half_f({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(_,{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
convert(attached_f,{dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss}) ->
    attached_f({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss});
convert(shell_f,{dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss}) ->
    shell_f({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss});
convert(_,{_,_},{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
select(Tag,List) ->
    set:select(Tag,List).
table(Tag,List) ->
    set:table(Tag,List).
side({side_t,Tab_t},Boundary,Region) ->
    set:get(set:get(Tab_t,Boundary),Region).
side({dual_t,Tab_t},{sides_f,Sides},Boundary,Region) ->
    Side = set:choose(Sides),
    Other = set:choose(set:remove(Sides,Side)),
    Sidedness = set:unionk(set:singleton({false,Side}),set:singleton({true,Other})),
    Boundaries = dual({dual_t,Tab_t},Side,Region),
    Member = set:member(Boundaries,Boundary),
    set:get(Sidedness,Member);
side({half_t,Tab_t},{sides_f,Sides},Boundary,Region) ->
    Side = set:choose(Sides),
    Other = set:choose(set:remove(Sides,Side)),
    Sidedness = set:unionk(set:singleton({false,Side}),set:singleton({true,Other})),
    Regions = half({half_t,Tab_t},Side,Boundary),
    Member = set:member(Regions,Region),
    set:get(Sidedness,Member).
side_f({dual_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Boundary,Region) ->
        side({dual_t,Tab_t},{sides_f,Sides},Boundary,Region) end,
    {side_f,set:tabular(Boundaries,Regions,Fun)};
side_f({half_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Boundary,Region) ->
        side({half_t,Tab_t},{sides_f,Sides},Boundary,Region) end,
    {side_f,set:tabular(Boundaries,Regions,Fun)}.
side_t({side_f,Tab_f}) ->
    Fun = fun({Boundary,Map}) ->
        {Boundary,set:tree(Map)} end,
    set:tree(set:map(Tab_f,Fun)).
side_b({side_t,Tab_t},Side,Boundary) ->
    fun(Region) ->
        side({side_t,Tab_t},Boundary,Region) == Side end.
side_r({side_t,Tab_t},Side,Region) ->
    fun(Boundary) ->
        side({side_t,Tab_t},Boundary,Region) == Side end.
dual({dual_t,Tab_t},Side,Region) ->
    set:get(set:get(Tab_t,Side),Region);
dual({side_t,Tab_t},{boundaries_f,Boundaries},Side,Region) ->
    Fun = side_r({side_t,Tab_t},Side,Region),
    set:filter(Boundaries,Fun).
dual_f({side_t,Tab_t},{boundaries_f,Boundaries},{side_f,Regions},{side_f,Sides}) ->
    Fun = fun(Side,Region) ->
        dual({side_t,Tab_t},{boundaries_f,Boundaries},Side,Region) end,
    {dual_f,set:tabulate(Sides,Regions,Fun)}.
dual_t({dual_f,Tab_f}) ->
    Fun = fun({Region,Set}) ->
        {Region,set:tree(Set)} end,
    set:tree(set:map(Tab,Fun)).
half({half_t,Tab_t},Side,Boundary) ->
    set:get(set:get(Tab_t,Boundary),Side).
half({side_t,Tab_t},{regions_f,Regions},Side,Boundary) ->
    Fun = side_b({side_t,Tab_t},Side,Boundary),
    set:filter(Regions,Fun).
half_f({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Side,Boundary) ->
        half({side_t,Tab_t},{side_f,Regions},Side,Boundary) end,
    {half_f,set:tabulate(Sides,Boundaries,Fun)}.
half_t({half_f,Tab_f}) ->
    Fun = fun({Boundary,Set}) ->
        {Boundary,set:tree(Set)} end,
    set:tree(set:map(Tab_f,Fun)).
duali({duali_t,Tab_t},Side,Boundaries) ->
    set:get(set:get(Tab_t,Side),Boundaries);
duali_f({dual_t,Tab_t},{sides_f,Sides}) ->
    Fun = fun(Side) ->
        set:inverse(set:get(Tab_t,Side)) end,
    {duali_f,set:tabulate(Sides,Fun)}.
duali_t({duali_f,Tab_f}) ->
    Fun = fun({Set,Region}) ->
        {set:tree(Set),Region} end,
    set:tree(set:map(Tab,Fun)).
halfi({halfi_t,Tab},Side,Regions) ->
    set:get(set:get(Tab,Side),Regions).
halfi_f({half_t,Tab_t},{sides_f,Sides}) ->
    Fun = fun(Side) ->
        set:inverse(set:get(Tab_t,Side)) end,
    {halfi_f,set:tabulate(Sides,Fun).
halfi_t({halfi_f,Tab_f}) ->
    Fun = fun({Set,Boundary}) ->
        {set:tree(Set),Boundary} end,
    set:tree(set:map(Tab,Fun)).
boundaries_f({side_f,Tab_f}) ->
    {boundaries_f,set:domain(Tag_f)};
boundaries_f({dual_f,Tab_f}) ->
    {boundaries_f,set:range(set:unionf(set:range(Tab_f)))};
boundaries_f({half_f,Tab_f}) ->
    {boundaries_f,set:domain(set:unionf(set:range(Tab_f)))}.
boundaries_t({boundaries_f,Tab_f}) ->
    {boundaries_t,set:tree(Tab_f)}.
regions_f({side_f,Tab_f}) ->
    {regions_f,set:domain(set:unionf(set:range(Tab_f)))};
regions_f({dual_f,Tab_f}) ->
    {regions_f,set:domain(set:unionf(set:range(Tab_f)))};
regions_f({half_f,Tab_f}) ->
    {regions_f,set:range(set:unionf(set:range(Tab_f)))}.
regions_t({regions_f,Tab_f}) ->
    {regions_t,set:tree(Tab_f)}.
sides_f({side_f,Tab_f}) ->
    {sides_f,set:range(set:unionf(set:range(Tab_f)))};
sides_f({dual_f,Tab_f}) ->
    {sides_f,set:domain(Tab_f)};
sides_f({half_f,Tab_f}) ->
    {sides_f,set:domain(Tab_f)}.
sides_t({sides_f,Tab_f}) ->
    {sides_t,set:tree(Tab_f)}.
length({boundaries_f,Tab_f}) ->
    {length,set:length(Tab_f)}.
linear({regions_f,Tab_f}) ->
    {linear,set:length({regions_f,Tab_f})}.
linear({dimension,_},{length,0}) ->
    {linear,1};
linear({dimension,0},{length,_}) ->
    {linear,1};
linear({dimension,Dim},{length,Len}) ->
    {linear,Left} = linear({dimension,Dim},{length,Len-1}),
    {linear,Right} = linear({dimension,Dim-1},{length,Len-1}),
    {linear,Left+Right}.
dimension({length,Len},{linear,Lin}) ->
    fun (true) ->
        {dimension,0};
    (false) ->
        {dimension,dimension_r(Len,Lin,1)} end
    (linear({dimension,0},{length,Len}) == {linear,Lin}).
dimension_r(Len,Lin,Dim) ->
    fun (true) ->
        Dim;
    (false) ->
        fun (true) ->
            dimension_r(Len,Lin,Dim,Dim div 2);
        (false) ->
            dimension_r(Len,Lin,Dim * 2) end
        (linear({dimension,Dim * 2},{length,Len}) > {linear,Lin}) end
    (linear({dimension,Dim},{length,Len}) == {linear,Lin}).
dimension_r(Len,Lin,Dim,Amt) ->
    fun (true) ->
        Dim+Amt;
    (false) ->
        fun (true) ->
            dimension_r(Len,Lin,Dim,Amt div 2);
        (false) ->
            dimension_r(Len,Lin,Dim+Amt,Amt div 2) end
        (linear({dimension,Dim+Amt},{length,Len}) > {linear,Lin}) end
    (linear({dimension,Dim+Amt},{length,Len}) == {linear,Lin}).
polyant({side_t,S_s},{regions_f,S_rs},
    {flat,Map}) -> %% quadrant?
    Fun = fun(Region) ->
        Fun = fun({Boundary,Side}) ->
            side({side_t,S_s},Boundary,Region) /= Side end,
        not set:any({flat,Map},Fun) end,
    set:filter(S_rs,Fun).
opposite({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
    {flat,Boundaries},Region) ->
    Side = set:choose(S_ss),
    Set = set:flat(set:get(S_d,Region)),
    Sym = set:symmetric({flat,Boundaries},Set),
    duali(S_di,Side,Sym).
neighbor({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
    Boundary,Region) ->
    Set = set:singleton(Boundary),
    Opposite = opposite({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
        Set,Region),
    set:choose(Opposite).
neighbor_b({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
    Boundary) ->
    fun(Region) ->
        try neighbor({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
                Boundary,Region) of _ ->
            true catch _ ->
            false end end.
neighbor_r({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
    Region) ->
    fun(Boundary) ->
        try neighbor({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
                Boundary,Region) of _ ->
            true catch _ ->
            false end end.
pencil({dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
    {flat,Boundaries}) ->
    Fun = fun(Region) ->
        try opposite({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
                {flat,Boundaries},Region) of _ ->
            true catch _ ->
            false end end,
    set:filter(S_rs,Fun).
corner({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{regions_f,S_rs),{sides_f,S_ss},
    {flat,Map}) ->
    Sup = polyant({side_t,S_s},{regions_f,S_rs},
        {flat,Map}),
    Boundaries = set:domain({flat,Map}),
    Sub = pencil({dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
        Boundaries),
    set:intersect(Sup,Sub).
attached({attached_t,S_a},Boundary) ->
    set:get(S_a,Boundary).
attached({dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
    Boundary) ->
    Fun = neighbor_b({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
        Boundary),
    set:filter(S_rs,Fun).
attached_f({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss}) ->
    Fun = fun(Boundary) ->
        attached({dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},Boundary) end,
    {attached_f,set:tabulate(S_bs,Fun)}.
attached_t({attached_f,S_a}) ->
    {attached_t,set:tree(S_a)}.
shell({shell_t,S_b},Region) ->
    set:get(S_b,Region).
shell({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},
    Region) ->
    Fun = neighbor_r({dual_t,S_d},{duali_t,S_di},{sides_f,S_ss},
        Region),
    set:filter(S_bs,Fun).
shell_f({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss}) ->
    Fun = fun(Region) ->
        shell({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},Region) end,
    {shell_f,set:tabulate(S_rs,Fun)}.
shell_t({shell_f,S_b}) ->
    {shell_t,set:tree(S_b)}.
attachedi({attachedi_t,Tab_t},Regions) ->
    set:get(Tab_t,Regions).
attachedi_f({attached_f,Tab_f}) ->
    {attachedi_f,set:inverse(Tab_f)}.
attachedi_t({attachedi_f,Tab_f}) ->
    {attachedi_t,set:tree(Tab_f)}.
shelli({shelli_t,Tab_t},Boundaries) ->
    set:get(Tab_t,Boundaries).
shelli_f({shell_f,Tab_f}) ->
    {shelli_f,set:inverse(Tab_f)}.
shelli_t({shelli_f,Tab_f}) ->
    {shelli_t,set:tree(Tab_f)}.
connected({dual_t,_},{duali_t,_},{boundaries_f,_},{sides_f,_},
    {flat,[]}) ->
    {flat,[]};
connected({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},
    {flat,Regions}) ->
    connected({dual_f,S_d},{duali_f,S_di},{boundaries,S_bs},{sides,S_ss},
        {flat,Regions},set:choose({flat,Regions})).
connected({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},
    {flat,Regions},Region) ->
    set:sort(
        {flat,
            connected_r({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},
                {flat,Regions},[],[])).
connected_r({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},
    {flat,Regions},[],Acc) ->
    Acc;
connected_r({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},
    {flat,Regions},[H|T],Acc) ->
    Shell = shell({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},H),
    {flat,Todo} = set:intersect(Shell,{flat,Regions}),
    Diff = set:difference({flat,Regions},{flat,Todo}),
    connected({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},Diff,T++Todo,[H|Acc]).
subspace({dual_t,S_d},{regions_f,S_rs},{sides_f,S_ss},
    {flat,Set}) ->
    Fun0 = fun(Region,Side) ->
        Get = set:convert([flat],set:get(set:get(S_d,Side),Region)),
        set:intersect({flat,Set},select(flat,Get)) end,
    Map0 = convert([tree],set:uniquefy_v(set:tabulate(S_rs,S_ss,Fun0))),
    Fun1 = fun(Side,Region) ->
        set:get(set:get(select(tree,Map0),Region),Side) end,
    {dual_f,set:tabulate(S_ss,set:domain(select(flat,Map0)),Fun1)}.
section({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
    {flat,[Boundary]}) ->
    Side = set:choose(S_ss),
    Attached = attached({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
        Side,Boundry),
    Fun = fun(Side,Region) ->
        Dual = dual({dual_t,S_d},Side,Region),
        set:remove(Dual,Boundary) end,
    {dual_f,set:tabulate(S_ss,Attached,Fun)};

section({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
    {flat,[Boundary|Boundaries]}) ->
    S = convert([side_t,dual_t,duali_t,regions_f,sides_f],
        section({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
        {flat,[Boundary]})),
    section(select(side_t,S),select(dual_t,S),select(duali_t,S),select(regions_f,S),select(sides_f,S),
        {flat,Boundaries}).
%% sections have same boundaries as space
subsection({side_t,_},{dual_t,_},{duali_t,_},
    {boundaries_f,_},{regions_f,_},{sides_f,_},
    [{dual_t,S0_d}],[{regions_f,_}],[{sides_f,_}]) ->
    S1 = convert([side_f],{dual_f,S0_d}),
    select(side_f,S1);
subsection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
    [{dual_t,S0_d},{dual_t,S1_d}],[{regions_f,S0_rs},{regions_f,S1_rs}],[{sides_f,S0_ss},{sides_f,S1_ss}]) ->
    Boundary = set:choose(S_bs),
    Boundaries = set:remove(S_bs,Boundary),
    S2 = convert([side_t,dual_t,duali_t,boundaries_f,regions_f,sides_f],
        subspace({dual_t,S_d},{regions_f,S_rs},{sides_f,S_ss},
            select(flat,Boundaries))),
    S3 = convert([dual_t,regions_f,sides_f],
        subspace({dual_t,S0_d},{regions_f,S0_rs},{sides_f,S0_ss},
            select(flat,Boundaries))),
    S4 = convert([dual_t,regions_f,sides_f],
        subspace({dual_t,S1_d},{regions_f,S1_rs},{sides_f,S1_ss},
            select(flat,Boundaries))),
    S5 = convert([dual_t,regions_f,sides_f],
        section({side_t,S_s},{dual_t,S_d},{duali_t,S_di},{regions_f,S_rs},{sides_f,S_ss},
            {flat,[Boundary]})),
    S6 = convert([side_t,dual_t,duali_t,boundaries_f,regions_f,sides_f],
        subsection(select(side_t,S2),select(dual_t,S2),select(duali_t,S2),
            select(boundaries_f,S2),select(regions_f,S2),select(sides_f,S2),
            [select(dual_t,S3),select(dual_t,S4)],
            [select(regions_f,S3),select(regions_f,S4)],
            [select(sides_f,S3),select(sides_f,S4)])),
    S7 = convert([duali_f],
        subsection(select(side_t,S2),select(dual_t,S2),select(duali_t,S2),
            select(boundaries_f,S2),select(regions_f,S2),select(sides_f,S2),
            [select(dual_t,S5),select(dual_t,S6)],
            [select(regions_f,S5),select(regions_f,S6)],
            [select(sides_f,S5),select(sides_f,S6)])),
    supersection(select(side_t,S6),select(dual_t,S6),select(duali_t,S6),
        select(boundaries_f,S6),select(regions_f,S6),select(sides_f,S6),
        select(duali_t,S7),Boundary);
subsection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
    {boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
    [{dual_t,S0_d}|S0s_d],[{regions_f,S0_rs}|S0s_rs],[{sides_f,S0_ss}|S0s_ss]) ->
    Fun = fun({dual_t,S1_d},{regions_f,S1_rs},{sides_f,S1_ss}) ->
        subsection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
            {boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
            [{dual_t,S0_d},{dual_t,S1_d}],
            [{regions_f,S0_rs},{regions_f,S1_rs}],
            [{sides_f,S0_ss},{sides_f,S1_ss}]) end,
    S1s_s = set:map({flat,S0s_d},{flat,S0s_rs},{flat,S0s_ss},Fun),
    subsection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
        {boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
        lists:map(set:bind1(dual_t,fun convert/2),S1s_s),
        lists:map(set:bind1(regions_f,fun convert/2),S1s_s),
        lists:map(set:bind1(sides_f,fun convert/2),S1s_s)).
%% sections have same boundaries as space
supersection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
    {boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
    {duali_t,S0_di},Boundary) ->
    Boundaries = set:insert(S_bs,Boundary),
    Side = set:choose(S_ss),
    Other = set:choose(set:remove(S_ss,Side)),
    Sidedness = set:unionk(set:singleton({false,Side}),set:singleton({true,Other})),
    Truth = set:inverse(Sidedness),
    Fun0 = fun(Region) ->
        Set = set:get(set:get(S_d,Side),Region),
        Map = set:get(S0_di,Side),
        set:memberk(Map,Set) end,
    Divided = set:filter(S_rs,Fun0),
    Diff = set:difference(S_rs,Divided),
    Half = connected({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},Diff),
    Fun1 = fun(Bound,Region) when Bound == Boundary ->
        set:get(Sidedness,set:member(Half,Region)); (_,_) ->
        set:get(set:get(S_s,Bound),Region) end,
    Ground = set:tabulate(Boundaries,S_rs,Fun1), %% regions on Side of Boundary if in Half
    Fun2 = fun(Bound,Region) when Bound == Boundary ->
        Side = set:get(set:get(Ground,Bound),Region),
        set:get(Sidedness,not set:get(Truth,Side)); (Bound,Region) ->
        set:get(set:get(Ground,Bound),Region) end,
    Figure = set:tabulate(Boundaries,Divided,Fun2), %% Divided to regions similar except opposite Boundary
    Holes = set:count(set:holes(S_rs,set:length(Divided))),
    Count = set:counti(Divided),
    Fun3 = fun({Region,Side}) ->
        {set:get(Holes,set:get(Count,Divided)),Side} end,
    Fun4 = fun(Bound) ->
        set:sort(set:map(set:get(Figure,Bound),Fun3)) end,
    %% return union of augmented regions and complementary divided
    {side_f,set:unionk(Ground,set:tabulate(Boundaries,Fun4))};
supersection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
    {boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
    [{side_t,S0_s}|S0s_s],[{dual_t,S0_d}|S0s_d],[{duali_t,S0_di}|S0s_di],
    [{boundaries_f,S0_bs}|S0s_bs],[{regions_f,S0_rs}|S0s_rs],[{sides_f,S0_ss}|S0s_ss],
    [Boundary|Boundaries]) ->
    Fun = fun({side_t,S1_s},{dual_t,S1_d},{duali_t,S1_di},
        {boundaries_bs,S1_bs},{regions_rs,S1_rs},{sides_ss,S1_ss}) ->
        S = convert([duali_t],
            subsection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
                {boundaries_f,S_bs},{regions_f,S_rs},{sides_F,S_ss},
                [{dual_t,S0_d},{dual_t,S1_d}],
                [{regions_f,S0_rs},{regions_f,S1_rs}],
                [{sides_f,S0_ss},{sides_f,S1_ss}])),
        supersection({side_t,S1_s},{dual_t,S1_d},{duali_t,S1_di},
            {boundaries_f,S1_bs},{regions_f,S1_rs},{sides_f,S1_ss},
            {duali_t,select(duali_t,S)},Boundary) end,
    S1s = lists:map(set:bind1([side_t,dual_t,duali_t,boundaries_f,regions_f,sides_f],fun convert/2),
        set:map({side_t,S0s_s},{dual_t,S0s_d},{duali_t,S0s_di},
        {boundaries_f,S0s_rs}{regions_f,S0s_rs},{sides_f,S0s_ss},Fun)),
    S1 = convert([side_t,dual_t,duali_t,boundaries_f,regions_f,sides_f],
        supersection({side_t,S_s},{dual_t,S_d},{duali_t,S_di},
            {boundaries_f,S_bs},{regions_f,S_rs},{sides_f,S_ss},
            {duali_t,S0_di},Boundary)),
    supersection(select(side_t,S1),select(dual_t,S1),select(duali_t,S1),
        select(boundaries_f,S1),select(regions_f,S1),select(sides_f,S1),
        lists:map(set:bind1(side_t,fun select/2),S1s),
        lists:map(set:bind1(dual_t,fun select/2),S1s),
        lists:map(set:bind1(duali_t,fun select/2),S1s),
        lists:map(set:bind1(boundaries_f,fun select/2),S1s),
        lists:map(set:bind1(regions_f,fun select/2),S1s),
        lists:map(set:bind1(sides_f,fun select/2),S1s),
        Boundaries).
canonical(List,1,{sides_f,Sides},{dimension,1}) ->
    Side = set:choose(Sides),
    Fun0 = fun Fun(Head,Tail,0) ->
        Head; (Head,{H|T},Num) ->
        Fun([H|Head],T,Num-1) end,
    Fun1 = fun(S,Region) when S == Side ->
        lists:reverse(Fun([],List,Region)); fun(_,Region) ->
        lists:nthtail(List,Region) end,
    {dual_f,set:tabulate(Sides,lists:seq(0,length-1),Fun1)};
canonical(List,Listl,{sides,Sides},{dimension,Dimension}) when Dimension == Listl ->
    ;
canonical(List,Listl,{sides,Sides},{dimension,Dimension}) when Dimension == Listl-1 ->
    .
orderings({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},{dimension,1}) ->
    Fun0 = fun(Region) ->
        set:length(shell({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},Region)) == 1 end,
    Start = set:choose(set:filter(S_rs,Fun0)),
    None = set:hole(S_bs),
    Fun1 = fun Fun([],_,Acc) ->
        Acc; Fun(Set,Region,Acc) ->
        Boundary = set:choose(Set),
        Neighbor = neighbor(S,Boundary,Regon),
        Shell = shell({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},Neighbor),
        Fun(set:remove(Shell,Boundary),Neighbor,[Boundary|Acc]) end,
    List = Fun1(shell({dual_t,S_d},{duali_t,S_di},{boundaries_f,S_bs},{sides_f,S_ss},Start),Start,[]),
    set:sort({flat,[List,lists:reverse(List)]}).
orderings({dual_f,S_d},{duali_f,S_di},{boundaries,S_bs},{sides,S_ss},{dimension,S_n}) ->
    .
superspace([{dual_t,S_d}|Ss_d],[{duali_t,S_di}|Ss_di],
    [{boundaries_f,S_bs}|Ss_bs],[{regions_f,_}|_],[{sides_f,S_ss}|Ss_ss],
    {dimension,1}) -> //6
    superspace_r([{dual_t,S_d}|Ss_d],[{duali_t,S_di}|Ss_di],
        [{boundaries_f,S_bs}|Ss_bs],[{sides_f,S_ss}|Ss_ss]); //4
superspace([{dual_t,S_d}|Ss_d],[{duali_t,_}|_],
    [{boundaries_f,_}|_],[{regions_f,S_rs}|Ss_rs],[{sides_f,S_ss}|Ss_ss],
    {dimension,_}) ->
    superspace_r([{dual_t,S_d}|Ss_d],[{regions_f,S_rs}|Ss_rs],[{sides_f,S_ss}|Ss_ss]). //3
superspace_r([{dual_t,S_d}],[{duali_t,_}],[{boundaries_f,_}],[{sides_f,_}]) -> //4
    dual_f({dual_t,S_d});
superspace_r([{dual_t,S0_d},{dual_t,S1_d}],[{duali_t,S0_di},{duali_t,S1_di}],
    [{boundaries_f,S0_bs},{boundaries_f,S1_bs}],[{sides_f,S0_ss},{sides_f,S1_ss}]) ->
    List0 = set:choose(orderings({dual_t,S0_d},{duali_t,S0_di},{boundaries_f,S0_bs},{sides_f,S0_ss},{dimension,1})),
    List1 = set:choose(orderings({dual_t,S1_d},{duali_t,S1_di},{boundaries_f,S1_bs},{sides_f,S1_ss},{dimension,1})),
    canonical({list,[List0,List1]},{length,2},S0_ss,{dimension,1});
superspace_r([{dual_t,S0_d},{dual_t,S1_d}|Ss_d],[{duali_t,S0_di},{duali_t,S1_di}|Ss_di],
    [{boundaries_f,S0_bs},{boundaries_f,S1_bs}|Ss_bs],[{sides_f,S0_ss},{sides_f,S1_ss}|Ss_ss]) ->
    Super = convert([dual_t,duali_t,boundaries_f,sides_f],
        superspace_r([{dual_t,S0_d},{dual_t,S1_d}],[{duali_t,S0_di},{duali_t,S1_di}],
            [{boundaries_f,S0_bs},{boundaries_F,S1_bs}],[{sides_f,S0_ss},{sides_f,S1_ss}])), //4
    superspace_r([select(dual_t,Super)|Ss_d],[select(duali_t,Super)|Ss_di],
        [select(boundaries_f,Super)|Ss_bs],[select(sides_f,Super),Ss_ss]). //4
superspace_r([{dual_t,S_d}],
    [{regions_f,_}],[{sides_f,_}]) -> //3
    dual_f({dual_t,S_d});
superspace_r([{dual_t,S0_d},{dual_t,S1_d}],
    [{regions_f,S0_rs},{regions_f,S1_rs}],[{sides_f,S0_ss},{sides_f,S1_ss}]) ->
    {flat,Shared} = set:intersect(S0_bs,S1_bs),
    {flat,Diff0} = set:difference(S0_bs,Shared),
    {flat,Diff1} = set:difference(S1_bs,Shared),
    superspace_r({flat,Shared},{dual_t,S0_d},{dual_t,S1_d},
        {regions_f,S0_rs},{regions_f,S1_rs},{sides_f,S0_ss},{sides_f,S1_ss},
        {flat,Diff0},{flat,Diff1},set:length(Diff0),set:length(Diff1)); //11
superspace_r([{dual_t,S0_d},{dual_t,S1_d}|Ss_d],
    [{regions_f,S0_rs},{regions_f,S1_rs}|Ss_rs],[{sides_f,S0_ss},{sides_f,S1_ss}|Ss_ss]) ->
    Super = convert([dual_t,regions_f,sides_f],
        superspace_r([{dual_t,S0_d},{dual_t,S1_d}],
            [{regions_f,S0_rs},{regions_f,S1_rs}],[{sides_f,S0_ss},{sides_f,S1_ss}])), //3
    superspace_r([select(dual_t,Super)|Ss_d],
        [select(regions_f,Super)|Ss_rs],[select(sides_f,Super),Ss_ss]). //3
superspace_r(_,{dual_t,S0_d},{dual_t,_},
    {regions_f,_},{regions_f,_},{sides_f,_},{sides_f,_},
    _,_,_,0) -> //11
    dual_f({dual_t,S0_d});
superspace_r(_,[{dual_t,_},{dual_t,S1_d}],
    [{regions_f,_},{regions_f,_}],[{sides_f,_},{sides_f,_}],
    _,_,0,_) ->
    {dual_f,S1_d};
superspace_r({flat,Shared},[{dual_t,S0_d},{dual_t,S1_d}],
    [{regions_f,S0_rs},{regions_f,S1_rs}],[{sides_f,S0_ss},{sides_F,S1_ss}],
    {flat,Diff0},{flat,Diff1},Length0,Length1) ->
    Bound0 = set:choose({flat,Diff0}),
    Bound1 = set:choose({flat,Diff1}),
    Bounds0 = convert([],set:insert({flat,Shared},Bound0)),
    Bounds1 = convert([],set:insert({flat,Shared},Bound1)),
    Sub = convert([side_f,dual_f,duali,boundaries_f,regions_f,sides_f],
        subspace({dual_f,S0_d},{regions,S0_rs},{sides,S0_ss},{flat,Shared})),
    Sub0 = convert([side_f,dual_f,duali,regions_f,sides_f],
        subspace({dual_f,S0_d},{regions,S0_rs},{sides,S0_ss},select(flat,Bounds0))),
    Sub1 = convert([side_f,dual_f,duali,regions_f,sides_f],
        subspace({dual_f,S1_d},{regions,S1_rs},{sides,S1_ss},select(flat,Bounds1))),
    Sect0 = convert([dual_f,duali,regions_f,sides_f],
        section(select(side,Sub0),select(dual,Sub0),select(duali,Sub0),
            select(regions,Sub0),select(sides,Sub0),Bound0)),
    Sect1 = convert([dual_f,duali,regions_f,sides_f],
        section(select(side,Sub1),select(dual,Sub1),select(duali,Sub1),
            select(regions,Sub1),select(sides,Sub1),Bound1)),
    Extend0 = convert([dual_f,regions_f,sides_f],
        supersection(select(side,Sub),select(dual,Sub),select(duali,Sub),
            select(boundaries,Sub),select(regions,Sub),select(sides,Sub),
            [select(dual,Sect0),select(dual,Sect1)],[select(duali,Sect0),select(duali,Sect1)],
            [select(regions,Sect0),select(regions,Sect1)],[select(sides,Sect0),select(sides,Sect1)],
            [Bound0,Bound1])),
    Extend1 = convert([dual_f,regions_f,sides_f],
        superspace_r([{dual_f,S0_d},select(dual,Extend0)],
            [{regions,S0_rs},select(regions,Extend0)],[{sides,S0_ss},select(sides,Extend0)])),
    superspace_r([select(dual,Extend1),{dual_f,S1_d}],
        [select(regions,Extend1),{regions,S1_rs}],[select(sides,Extend1),{sides,S1_ss}]).
