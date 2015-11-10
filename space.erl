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
-export([polyant/3,neighbor/5,neighbor/5]).
-export([pencil/5,corner/6]).
-export([attached/2,attached/5,shell/2,shell/5]).
-export([attachedi/2,shelli/2]).
-export([connected/5,connected/6]).
-export([sign/7,axis/7,facet/7]).
%% functions for spaces in same universe
-export([subspace/4,section/6,subsection/9]).
-export([supersection/8,supersecton/13]).
-export([canonical/4,orderings/5,superspace/6]).
convert([dual_tf,regions_f,sides_f],{dual_ff,S_dff}) ->
    [{dual_ff,_},{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}] = convert(
        [dual_tf,regions_f,sides_f],{dual_ff,S_dff}),
    [{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}];
convert([dual_tf,duali_tf,regions_f,sides_f],{dual_ff,S_dff}) ->
    [{dual_ff,_},{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},{duali_ff,_},{duali_tf,S_ditf}] = convert(
        [dual_tf,regions_f,sides_f, dual_tf,sides_f,duali_ff, duali_ff,duali_tf],{dual_ff,S_dff}),
    [{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf}];
convert([dual_tf,duali_tf,boundaries_f,sides_f],{dual_ff,S_dff}) ->
    [{dual_ff,_},{dual_tf,S_dtf},{boundaries_f,S_bf},{sides_f,S_pf},{duali_ff,_},{duali_tf,S_ditf}] = convert(
        [dual_tf,boundaries_f,sides_f, dual_tf,sides_f,duali_ff, duali_ff,duali_tf],{dual_ff,S_dff}),
    [{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf}];
convert([side_t,dual_tf,duali_tf,regions_f,sides_f],{dual_ff,S_dff}) ->
    [{dual_ff,_},{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},{duali_ff,_},
        {duali_tf,S_ditf},{side_f,S_sf},{side_t,S_st}] = convert(
        [dual_tf,regions_f,sides_f, dual_tf,sides_f,duali_ff, duali_ff,duali_tf,
            dual_tf,boundaries_f,regions_f,sides_f,side_f, side_f,side_t],{dual_ff,S_dff}),
    [{side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf}];
convert([side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],{dual_ff,S_dff}) ->
    [{dual_ff,_},{dual_tf,S_dtf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        {dual_tf,S_dtf},{duali_ff,_},{duali_tf,S_ditf},{side_f,_},{side_t,S_st}] = convert(
        [dual_tf,boundaries_f,regions_f,sides_f, dual_tf,duali_ff, duali_ff,duali_tf,
            dual_tf,boundaries_f,regions_f,sides_f,side_f, side_f,side_t],{dual_ff,S_dff}),
    [{side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}];
convert([duali_tf],{side_f,S_pf}) ->
    [{side_f,_},{dual_ff,_},{duali_ff,_},{duali_tf,S_ditf}] = convert(
        [dual_ff,dual_ff,duali_ff,duali_ff,duali_tf],{dual_ff,S_dff}),
    [{duali_tf}];
convert([dual_tf,regions_f,sides_f],{side_f,S_pf}) ->
    [{side_f,_},{side_t,_},{boundaries_f,_},{regions_f,_},{sides_f,_},{dual_ff,_},{dual_tf,S_dtf}] = convert(
        [side_f,side_t,boundaries_f,regions_f,sides_f,
            side_t,boundaries_f,regions_f,sides_f,dual_ff,
            dual_ff,dual_tf],{dual_ff,S_dff}),
    [{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}];
convert([side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],{side_f,S_pf}) ->
    [{side_f,_},{side_t,S_st},{dual_ff,_},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        {dual_tf,S_dtf},{duali_ff,_},{duali_tf,S_ditf}] = convert(
        [side_t,dual_ff,boundaries_f,regions_f,sides_f, dual_ff,dual_tf,duali_ff, duali_ff,duali_tf],
        {side_f,S_pf}),
    [{side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}];
convert([H|T],{Tag,Tab}) ->
    lambda:convert([H|T],{Tag,Tab},[{Tag,Tab}],false,fun convert/2,fun convert/3,fun convert/4,fun convert/5,fun convert/6);
convert(side_t,{side_f,Tab_f}) ->
    {side_t,set:deept({flat,Tab_f})};
convert(side_f,{side_t,Tab_t}) ->
    {side_f,set:deepf({tree,Tab_t})};
convert(dual_tf,{dual_ff,Tab_f}) ->
    {dual_tf,set:deept({flat,Tab_f})};
convert(dual_ff,{dual_tf,Tab_t}) ->
    {dual_ff,set:deepf({tree,Tab_t})};
convert(half_tf,{half_ff,Tab_f}) ->
    {half_tf,set:deept({flat,Tab_f})};
convert(half_ff,{half_tf,Tab_t}) ->
    {half_ff,set:deepf({tree,Tab_t})};
convert(duali_tf,{duali_ff,Tab_f}) ->
    {duali_tf,set:deept({flat,Tab_f})};
convert(duali_ff,{duali_tf,Tab_t}) ->
    {duali_ff,set:deepf({tree,Tab_t})};
convert(halfi_tf,{halfi_ff,Tab_f}) ->
    {halfi_tf,set:deept({flat,Tab_f})};
convert(halfi_ff,{halfi_tf,Tab_t}) ->
    {halfi_ff,set:deepf({tree,Tab_t})};
convert(boundaries_t,{boundaries_f,Tab_f}) ->
    {boundaries_t,set:deept({flat,Tab_f})};
convert(boundaries_f,{boundaries_t,Tab_t}) ->
    {boundaries_f,set:deepf({tree,Tab_t})};
convert(regions_t,{regions_f,Tab_f}) ->
    {regions_t,set:deept({flat,Tab_f})};
convert(regions_f,{regions_t,Tab_t}) ->
    {regions_f,set:deepf({tree,Tab_t})};
convert(sides_t,{sides_f,Tab_f}) ->
    {sides_t,set:deept({flat,Tab_f})};
convert(sides_f,{sides_t,Tab_t}) ->
    {sides_f,set:deepf({tree,Tab_t})};
convert(attached_tf,{attached_ff,Tab_f}) ->
    {attached_tf,set:deept({flat,Tab_f})};
convert(attached_ff,{attached_tf,Tab_t}) ->
    {attached_ff,set:deepf({tree,Tab_t})};
convert(shell_tf,{shell_ff,Tab_f}) ->
    {shell_tf,set:deept({flat,Tab_f})};
convert(shell_ff,{shell_tf,Tab_t}) ->
    {shell_ff,set:deepf({tree,Tab_t})};
convert(attachedi_tf,{attachedi_ff,Tab_f}) ->
    {attachedi_tf,set:deept({flat,Tab_f})};
convert(attachedi_ff,{attachedi_tf,Tab_t}) ->
    {attachedi_ff,set:deepf({tree,Tab_t})};
convert(shelli_tf,{shelli_ff,Tab_f}) ->
    {shelli_tf,set:deept({flat,Tab_f})};
convert(shelli_ff,{shelli_tf,Tab_t}) ->
    {shelli_ff,set:deepf({tree,Tab_t})};
convert(boundaries_f,{side_f,Tab_f}) ->
    boundaries({side_f,Tab_f});
convert(boundaries_f,{dual_ff,Tab_f}) ->
    boundaries({dual_ff,Tab_f});
convert(boundaries_f,{half_ff,Tab_f}) ->
    boundaries({half_ff,Tab_f});
convert(regions_f,{side_f,Tab_f}) ->
    regions({side_f,Tab_f});
convert(regions_f,{dual_ff,Tab_f}) ->
    regions({dual_ff,Tab_f});
convert(regions_f,{half_ff,Tab_f}) ->
    regions({half_ff,Tab_f});
convert(sides_f,{side_f,Tab_f}) ->
    sides({side_f,Tab_f});
convert(sides_f,{dual_ff,Tab_f}) ->
    sides({dual_ff,Tab_f});
convert(sides_f,{half_ff,Tab_f}) ->
    sides({half_ff,Tab_f});
convert(length,{boundaries_f,Tab_f}) ->
    length({boundaries_f,Tab_f});
convert(linear,{regions_f,Tab_f}) ->
    linear({regions_f,Tab_f});
convert(attached_tf,{attached_ff,Tab_f}) ->
    attached_tf({attached_ff,Tab_f});
convert(shell_tf,{shell_ff,Tab_f}) ->
    shell_tf({shell_ff,Tab_f});
convert(attachedi_ff,{attached_ff,Tab_f}) ->
    attachedi_ff({attached_ff,Tab_f});
convert(attachedi_tf,{attachedi_ff,Tab_f}) ->
    attachedi_tf({attachedi_ff,Tab_f});
convert(shelli_ff,{shell_ff,Tab_f}) ->
    shelli_ff({shell_ff,Tab_f});
convert(shelli_tf,{shelli_ff,Tab_f}) ->
    shelli_tf({shelli_ff,Tab_f});
convert(_,{_,_}) ->
    throw([]).
convert(duali_ff,{dual_tf,Tab_t},{sides_f,Sides}) ->
    duali_ff({dual_tf,Tab_t},{sides_f,Sides});
convert(halfi_ff,{half_tf,Tab_t},{sides_f,Sides}) ->
    halfi_ff({half_tf,Tab_t},{sides_f,Sides});
convert(_,{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_}) ->
    throw([]).
convert(side_f,{dual_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    side_f({dual_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(side_f,{half_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    side_f({half_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(dual_ff,{side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    dual_ff({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(half_ff,{side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    half_ff({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides});
convert(_,{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
convert(attached_ff,{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) ->
    attached_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf});
convert(shell_ff,{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) ->
    shell_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf});
convert(_,{_,_},{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
side({side_t,Tab_t},Boundary,Region) ->
    set:get(set:get({tree,Tab_t},Boundary),Region).
side({dual_tt,Tab_t},{sides_f,Sides},Boundary,Region) ->
    Side = set:choose({flat,Sides}),
    Other = set:choose(set:remove({flat,Sides},Side)),
    {tree,Sidedness} = set:tree(set:unionk(set:singleton({false,Side}),set:singleton({true,Other}))),
    {tree,Boundaries} = dual({dual_tt,Tab_t},Side,Region),
    Member = set:member({tree,Boundaries},Boundary),
    set:get({tree,Sidedness},Member);
side({half_tt,Tab_t},{sides_f,Sides},Boundary,Region) ->
    Side = set:choose({flat,Sides}),
    Other = set:choose(set:remove({flat,Sides},Side)),
    {tree,Sidedness} = set:tree(set:unionk(set:singleton({false,Side}),set:singleton({true,Other}))),
    {tree,Regions} = half({half_tt,Tab_t},Side,Boundary),
    Member = set:member({tree,Regions},Region),
    set:get({tree,Sidedness},Member).
side_f({dual_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Boundary,Region) ->
        side({dual_tt,Tab_t},{sides_f,Sides},Boundary,Region) end,
    {side_f,lambda:tabulate(Boundaries,Regions,Fun)};
side_f({half_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Boundary,Region) ->
        side({half_tt,Tab_t},{sides_f,Sides},Boundary,Region) end,
    {side_f,lambda:tabulate(Boundaries,Regions,Fun)}.
side_b({side_t,Tab_t},Side,Boundary) ->
    fun(Region) ->
        side({side_t,Tab_t},Boundary,Region) == Side end.
side_r({side_t,Tab_t},Side,Region) ->
    fun(Boundary) ->
        side({side_t,Tab_t},Boundary,Region) == Side end.
dual({dual_tf,Tab_t},Side,Region) ->
    set:get(set:get({tree,Tab_t},Side),Region);
dual({side_t,Tab_t},{boundaries_f,Boundaries},Side,Region) ->
    Fun = side_r({side_t,Tab_t},Side,Region),
    {flat,lists:filter(Fun,Boundaries)}.
dual_ff({side_t,Tab_t},{boundaries_f,Boundaries},{side_f,Regions},{side_f,Sides}) ->
    Fun = fun(Side,Region) ->
        dual({side_t,Tab_t},{boundaries_f,Boundaries},Side,Region) end,
    {dual_ff,lambda:tabulate(Sides,Regions,Fun)}.
half({half_tf,Tab_t},Side,Boundary) ->
    set:get(set:get({tree,Tab_t},Boundary),Side).
half({side_t,Tab_t},{regions_f,Regions},Side,Boundary) ->
    Fun = side_b({side_t,Tab_t},Side,Boundary),
    {flat,lists:filter(Fun,Regions)}.
half_ff({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Side,Boundary) ->
        half({side_t,Tab_t},{side_f,Regions},Side,Boundary) end,
    {half_ff,lambda:tabulate(Sides,Boundaries,Fun)}.
duali({duali_tf,Tab_t},Side,Boundaries) ->
    set:get(set:get({tree,Tab_t},Side),Boundaries);
duali_ff({dual_tf,Tab_t},{sides_f,Sides}) ->
    Fun = fun(Side) ->
        set:inverse(set:get({tree,Tab_t},Side)) end,
    {duali_ff,lambda:tabulate(Sides,Fun)}.
halfi({halfi_tf,Tab},Side,Regions) ->
    set:get(set:get({tree,Tab},Side),Regions).
halfi_ff({half_tf,Tab_t},{sides_f,Sides}) ->
    Fun = fun(Side) ->
        set:inverse(set:flat(set:get({tree,Tab_t},Side))) end,
    {halfi_ff,lambda:tabulate(Sides,Fun)}.
boundaries({side_f,Tab_f}) ->
    {boundaries_f,set:list(set:domain({flat,Tab_f}))};
boundaries({dual_ff,Tab_f}) ->
    {boundaries_f,set:list(set:range(set:unionf(set:range({flat,Tab_f}))))};
boundaries({half_ff,Tab_f}) ->
    {boundaries_f,set:list(set:domain(set:unionf(set:range({flat,Tab_f}))))}.
regions({side_f,Tab_f}) ->
    {regions_f,set:list(set:domain(set:unionf(set:range({flat,Tab_f}))))};
regions({dual_ff,Tab_f}) ->
    {regions_f,set:list(set:domain(set:unionf(set:range({flat,Tab_f}))))};
regions({half_ff,Tab_f}) ->
    {regions_f,set:list(set:range(set:unionf(set:range({flat,Tab_f}))))}.
sides({side_f,Tab_f}) ->
    {sides_f,set:list(set:range(set:unionf(set:range({flat,Tab_f}))))};
sides({dual_ff,Tab_f}) ->
    {sides_f,set:list(set:domain({flat,Tab_f}))};
sides({half_ff,Tab_f}) ->
    {sides_f,set:list(set:domain({flat,Tab_f}))}.
length({boundaries_f,Tab_f}) ->
    {length,set:length({flat,Tab_f})}.
linear({regions_f,Tab_f}) ->
    {linear,set:length({flat,Tab_f})}.
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
polyant({side_t,S_st},{regions_f,S_rf},
    {flat,Map}) -> %% quadrant?
    Fun = fun(Region) ->
        Fun = fun({Boundary,Side}) ->
            side({side_t,S_st},Boundary,Region) /= Side end,
        not lists:any(Fun,Map) end,
    {flat,lists:filter(Fun,S_rs)}.
neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
    {flat,Boundaries},Region) ->
    Side = set:choose({flat,S_pf}),
    {flat,Set} = set:flat(set:get({tree,S_dtf},Region)),
    {flat,Sym} = set:symmetric({flat,Boundaries},{flat,Set}),
    duali({duali_tf,S_ditf},Side,{flat,Sym}).
neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
    Boundary,Region) ->
    {flat,Set} = set:singleton(Boundary),
    {flat,Neighbor} = neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
        {flat,Set},Region),
    set:choose({flat,Neighbor}).
neighbor_b({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
    Boundary) ->
    fun(Region) ->
        try neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
                Boundary,Region) of _ ->
            true catch _ ->
            false end end.
neighbor_r({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
    Region) ->
    fun(Boundary) ->
        try neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
                Boundary,Region) of _ ->
            true catch _ ->
            false end end.
pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    {flat,Boundaries}) ->
    Fun = fun(Region) ->
        try neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
                {flat,Boundaries},Region) of _ ->
            true catch _ ->
            false end end,
    {flat,lists:filter(Fun,S_rs)}.
corner({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rs),{sides_f,S_pf},
    {flat,Map}) ->
    {flat,Sup} = polyant({side_t,S_st},{regions_f,S_rf},
        {flat,Map}),
    {flat,Boundaries} = set:domain({flat,Map}),
    {flat,Sub} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
        {flat,Boundaries}),
    set:intersect({flat,Sup},{flat,Sub}).
attached({attached_tf,S_atf},Boundary) ->
    set:get({tree,S_atf},Boundary).
attached({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    Boundary) ->
    Fun = neighbor_b({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
        Boundary),
    {flat,lists:filter(Fun,S_rs)}.
attached_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) ->
    Fun = fun(Boundary) ->
        set:list(attached({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},Boundary)) end,
    {attached_ff,lambda:tabulate(S_bs,Fun)}.
shell({shell_tf,S_btf},Region) ->
    set:get({tree,S_btf},Region).
shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
    Region) ->
    Fun = neighbor_r({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
        Region),
    {flat,lists:filter(Fun,S_bs)}.
shell_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) ->
    Fun = fun(Region) ->
        shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},Region) end,
    {shell_ff,lambda:tabulate(S_rs,Fun)}.
attachedi({attachedi_tf,Tab_t},Regions) ->
    set:get({tree,Tab_t},Regions).
attachedi_ff({attached_ff,Tab_f}) ->
    {attachedi_ff,set:list(set:inverse({flat,Tab_f}))}.
shelli({shelli_tf,Tab_t},Boundaries) ->
    set:get({tree,Tab_t},Boundaries).
shelli_ff({shell_ff,Tab_f}) ->
    {shelli_ff,set:list(set:inverse({flat,Tab_f}))}.
connected({dual_tf,_},{duali_tf,_},{boundaries_f,_},{sides_f,_},
    {flat,[]}) ->
    {flat,[]};
connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
    {flat,Regions}) ->
    connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
        {flat,Regions},set:choose({flat,Regions})).
connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
    {flat,Regions},Region) ->
    set:sort(
        connected_r({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
            {flat,Regions},[],[])).
connected_r({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
    {flat,Regions},[],Acc) ->
    Acc;
connected_r({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
    {flat,Regions},[H|T],Acc) ->
    {flat,Shell} = shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},H),
    {flat,Todo} = set:intersect({flat,Shell},{flat,Regions}),
    {flat,Diff} = set:difference({flat,Regions},{flat,Todo}),
    connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Diff,T++Todo,[H|Acc]).
sign({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    Boundary,{flat,Vertex}) ->
    {flat,Regions} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
        {flat,Vertex}),
    Region = set:choose({flat,Regions}),
    side({side_t,S_st},Boundary,Region).
axis({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    {flat,V0},{flat,V1}) -> %% one to one map from pencil of vertex V0 to pencil of V1
    Side = set:choose({flat,S_pf}),
    Other = set:choose(set:difference({flat,S_pf},set:singleton(Side))),
    Xor = fun(Side,false) ->
        Side;
    (Side,true) ->
        Other;
    (Other,false) ->
        Other;
    (Other,true) ->
        Side end,
    {flat,P0} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,V0}),
    {flat,P1} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,V1}),
    R0 = set:choose({flat,P0}),
    Wrap = fun(Region,Boundary) ->
        side({side_t,S_st},Boundary,Region) end,
    Conv0 = fun({B0,{B1,Side}}) ->
        {B0,Xor(Side,(side({side_t,S_st},B0,R0)!=side({side_t,S_st},B1,R0)))} end,
    Conv1 = fun(R1,{flat,Map}) ->
        {R1,lists:map(Conv0,lambda:zip(V0,Map))} end,
    {tree,Map0} = set:tree({flat,lambda:tabulate(P0,V0,Wrap)}),
    {flat,Map} = {flat,lambda:tabulate(P1,V1,Wrap)},
    {tree,Map1} = set:tree(set:inverse({flat,lists:map(Conv1,Map)})),
    Fun = fun(Region) ->
        Map = set:get({tree,Map0},Region),
        set:get({tree,Map1},Map) end,
    {flat,lists:map(Fun,P0)}.
facet({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    Boundary,Region) -> %% shell of region in section by boundary
    [{dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,S0_bf},{sides_f,S0_pf}] = convert(
        [dual_tf,duali_tf,boundaries_f,sides_f],
        section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
            {flat,[Boundary]})),
    {flat,Dual} = dual({dual_tf,S_dtf},Region),
    {flat,Diff} = set:difference({flat,Dual},set:singleton(Boundary)),
    Take = duali({duali_tf,S0_ditf},{flat,Diff}),
    shell({dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,S0_bf},{sides_f,S0_pf},Take).
subspace({dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},
    {flat,Set}) ->
    Fun0 = fun(Region,Side) ->
        {flat,Get} = set:convert(flat,set:get(set:get({tree,S_dtf},Side),Region)),
        set:tree(set:intersect({flat,Set},{flat,Get})) end,
    {flat,Map0} = set:uniquefy_v(set:tabulate(S_rs,S_pf,Fun0)),
    {tree,Map1} = set:deept({flat,Map0}),
    Fun1 = fun(Side,Region) ->
        set:convert(flat,set:get(set:get({tree,Map1},Region),Side)) end,
    {dual_ff,lambda:tabulate(S_pf,set:list(set:domain({flat,Map0})),Fun1)}.
section({side_t,_},{dual_tf,S_dtf},{duali_tf,_},{regions_f,_},{sides_f,_},
    {flat,[]}) ->
    convert(dual_ff,{dual_tf,S_dtf});
section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    {flat,[Boundary]}) ->
    Side = set:choose(S_pf),
    {tree,Map} = set:get({tree,S_st},Boundary),
    Fun0 = fun(Region) ->
        set:get({tree,Map},Region) == Side end,
    {flat,List} = attached({dual_tf,S_dtf},{duali_tf,S_ditf},
            {regions_f,S_rf},{sides_f,S_pf},Boundary),
    {flat,Attached} = {flat,lists:filter(Fun0,List)},
    Fun1 = fun(Side,Region) ->
        {flat,Dual} = dual({dual_tf,S_dtf},Side,Region),
        set:remove({flat,Dual},Boundary) end,
    {dual_ff,lambda:tabulate(S_pf,Attached,Fun1)};
section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
    {flat,[Boundary|Boundaries]}) ->
    [{side_t,S0_st},{dual_tf,S0_dtf},{duali_tf,S0_ditf},{regions_f,S0_rf},{sides_f,S0_pf}] = convert(
        [side_t,dual_tf,duali_tf,regions_f,sides_f],
        section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
            {flat,[Boundary]})),
    section({side_t,S0_st},{dual_tf,S0_dtf},{duali_tf,S0_ditf},{regions_f,S0_rf},{sides_f,S0_pf},
        {flat,Boundaries}).
%% sections have same boundaries as space
subsection({side_t,S_st},{dual_tf,_},{duali_tf,_},
    {boundaries_f,_},{regions_f,_},{sides_f,_},
    [],[],[]) ->
    convert(side_f,{side_t,S_st});
subsection({side_t,_},{dual_tf,_},{duali_tf,_},
    {boundaries_f,_},{regions_f,_},{sides_f,_},
    [{dual_tf,S0_dtf}],[{regions_f,S0_rf}],[{sides_f,S0_pf}]) ->
    {dual_ff,S0_dff} = convert(dual_ff,{dual_tf,S0_dtf}),
    {boundaries_f,S0_bf} = convert(boundaries_f,{dual_ff,S0_dff}),
    convert(side_f,{dual_tf,S0_dtf},{boundaries_f,S0_bf},{regions_f,S0_rf},{sides_f,S0_pf});
subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{dual_tf,S0_dtf},{dual_tf,S1_dtf}],[{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}]) ->
    Boundary = set:choose({flat,S_bf}),
    {flat,Boundaries} = set:remove({flat,S_bf},Boundary),
    [{side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},{boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf}] = convert(
        [side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],
        subspace({dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},
            {flat,Boundaries})),
    [{dual_tf,S3_dtf},{regions_f,S3_rf},{sides_f,S3_pf}] = convert(
        [dual_tf,regions_f,sides_f],
        subspace({dual_tf,S0_dtf},{regions_f,S0_rf},{sides_f,S0_pf},
            {flat,Boundaries})),
    [{dual_tf,S4_dtf},{regions_f,S4_rf},{sides_f,S4_pf}] = convert(
        [dual_tf,regions_f,sides_f],
        subspace({dual_tf,S1_dtf},{regions_f,S1_rf},{sides_f,S1_pf},
            {flat,Boundaries})),
    [{dual_tf,S5_dtf},{regions_f,S5_rf},{sides_f,S5_pf}] = convert(
        [dual_tf,regions_f,sides_f],
        section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
            {flat,[Boundary]})),
    [{side_t,S6_st},{dual_tf,S6_dtf},{duali_tf,S6_ditf},
        {boundaries_f,S6_bf},{regions_f,S6_rf},{sides_f,S6_pf}] = convert(
        [side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],
        subsection({side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
            {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf},
            [{dual_tf,S3_dtf},{dual_tf,S4_dtf}],
            [{regions_f,S3_rf},{regions_f,S4_rf}],
            [{sides_f,S3_pf},{sides_f,S4_pf}])),
    [{duali_tf,S7}] = convert([duali_tf],
        subsection({side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
            {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf},
            [{dual_tf,S5_dtf},{dual_tf,S6_dtf}],
            [{regions_f,S5_rf},{regions_f,S6_rf}],
            [{sides_f,S5_pf},{sides_f,S6_pf}])),
    supersection({side_t,S6_st},{dual_tf,S6_dtf},{duali_tf,S6_ditf},
        {boundaries_f,S6_bf},{regions_f,S6_rf},{sides_f,S6_pf},
        {duali_tf,S7_ditf},Boundary);
subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
    {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{dual_tf,S0_dtf}|S0s_d],[{regions_f,S0_rf}|S0s_rs],[{sides_f,S0_pf}|S0s_pf]) ->
    Fun = fun({{dual_tf,S1_dtf},{regions_f,S1_rf},{sides_f,S1_pf}}) ->
        subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
            {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
            [{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
            [{regions_f,S0_rf},{regions_f,S1_rf}],
            [{sides_f,S0_pf},{sides_f,S1_pf}]) end,
    S1s_s = lists:map(Fun,lambda:zip(S0s_d,S0s_rs,S0s_pf)),
    subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
        {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        lists:map(fun(S1_s) -> convert(dual_tf,S1_s) end,S1s_s),
        lists:map(fun(S1_s) -> convert(regions_f,S1_s) end,S1s_s),
        lists:map(fun(S1_s) -> convert(sides_f,S1_s) end,S1s_s)).
%% sections have same boundaries as space
supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
    {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    {duali_tf,S0_ditf},Boundary) ->
    {flat,Boundaries} = set:insert(S_bs,Boundary),
    Side = set:choose({flat,S_pf}),
    Other = set:choose(set:remove({flat,S_pf},Side)),
    {tree,Sidedness} = tree(set:unionk(set:singleton({false,Side}),set:singleton({true,Other}))),
    {tree,Truth} = tree(set:inverse({flat,Sidedness})),
    Fun0 = fun(Region) ->
        {flat,Set} = set:get(set:get(S_d,Side),Region),
        {tree,Map} = set:get(S0_di,Side),
        set:memberk({tree,Map},{flat,Set}) end,
    {flat,Divided} = {flat,lists:filter(Fun0,S_rs)},
    {flat,Diff} = set:difference({flat,S_rf},{flat,Divided}),
    {tree,Half} = tree(connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Diff})),
    Fun1 = fun(Bound,Region) when Bound == Boundary ->
        set:get({tree,Sidedness},set:member({tree,Half},Region)); (_,_) ->
        set:get(set:get({tree,S_st},Bound),Region) end,
    {flat,Ground} = {flat,lambda:tabulate(Boundaries,S_rs,Fun1)}, %% regions on Side of Boundary if in Half
    Fun2 = fun(Bound,Region) when Bound == Boundary ->
        Side = set:get(set:get(Ground,Bound),Region),
        set:get(Sidedness,not set:get(Truth,Side)); (Bound,Region) ->
        set:get(set:get(Ground,Bound),Region) end,
    {tree,Figure} = set:tree({flat,lambda:tabulate(Boundaries,Divided,Fun2)}), %% Divided to regions similar except opposite Boundary
    {tree,Map} = set:tree(set:sortk(lambda:zip(Divided,set:holes({flat,S_rf},set:length({flat,Divided}))))),
    Fun3 = fun({Region,Side}) ->
        {set:get({tree,Map},Region),Side} end,
    %% {tree,Holes} = set:tree(set:count(set:holes({flat,S_rf},set:length({flat,Divided})))),
    %% {tree,Count} = set:tree(set:counti({flat,Divided})),
    %% Fun3 = fun({Region,Side}) ->
    %%     {set:get({tree,Holes},set:get({tree,Count},{flat,Divided})),Side} end,
    Fun4 = fun(Bound) ->
        set:sort(lists:map(Fun3,set:get({tree,Figure},Bound))) end,
    %% return union of augmented regions and complementary divided
    {side_f,set:list(set:unionk({flat,Ground},{flat,lambda:tabulate(Boundaries,Fun4)}))};
supersection({side_t,S_st},{dual_tf,_},{duali_tf,_},
    {boundaries_f,_},{regions_f,_},{sides_f,_},
    [],[],[],[],[],[],[]) ->
    convert(side_f,{side_t,S_st});
supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
    {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{side_t,S0_st}],[{dual_tf,S0_dtf}],[{duali_tf,S0_ditf}],
    [{boundaries_f,S0_bf}],[{regions_f,S0_rf}],[{sides_f,S0_pf}],
    [Boundary]) ->
    supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
        {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        {duali_tf,S0_ditf},Boundary);
supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
    {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{side_t,S0_st}|S0s_s],[{dual_tf,S0_dtf}|S0s_d],[{duali_tf,S0_ditf}|S0s_di],
    [{boundaries_f,S0_bf}|S0s_bs],[{regions_f,S0_rf}|S0s_rs],[{sides_f,S0_pf}|S0s_pf],
    [Boundary|Boundaries]) ->
    Fun = fun({{side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
        {boundaries_bs,S1_bf},{regions_rs,S1_rf},{sides_pf,S1_pf}}) ->
        {duali_tf,S2_ditf} = convert([duali_tf],
            subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
                {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
                [{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
                [{regions_f,S0_rf},{regions_f,S1_rf}],
                [{sides_f,S0_pf},{sides_f,S1_pf}])),
        supersection({side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
            {boundaries_f,S1_bf},{regions_f,S1_rf},{sides_f,S1_pf},
            {duali_tf,S2_ditf},Boundary) end,
    S1s = lists:map(fun(S0) -> convert([side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],S0) end,
        lists:map(Fun,lambda:zip(S0s_s,S0s_d,S0s_di,S0s_bs,S0s_rs,S0s_pf))),
    [{side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
        {boundaries_f,S1_bf},{regions_f,S1_rf},{sides_f,S1_pf}] = convert(
        [side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],
        supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
            {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
            {duali_tf,S0_ditf},Boundary)),
    supersection({side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
        {boundaries_f,S1_bf},{regions_f,S1_rf},{sides_f,S1_rf},
        lists:map(fun(S1) -> convert(side_t,S1) end,S1s),
        lists:map(fun(S1) -> convert(dual_tf,S1) end,S1s),
        lists:map(fun(S1) -> convert(duali_tf,S1) end,S1s),
        lists:map(fun(S1) -> convert(boundaries_f,S1) end,S1s),
        lists:map(fun(S1) -> convert(regions_f,S1) end,S1s),
        lists:map(fun(S1) -> convert(sides_f,S1) end,S1s),
        Boundaries).
canonical(List,1,{sides_f,Sides},{dimension,1}) ->
    Side = set:choose(Sides),
    Fun0 = fun Fun(Head,Tail,0) ->
        Head; (Head,{H|T},Num) ->
        Fun([H|Head],T,Num-1) end,
    Fun1 = fun(S,Region) when S == Side ->
        lists:reverse(Fun([],List,Region)); fun(_,Region) ->
        lists:nthtail(List,Region) end,
    {dual_ff,lambda:tabulate(Sides,lists:seq(0,length-1),Fun1)};
canonical(List,Listl,{sides,Sides},{dimension,Dimension}) when Dimension == Listl ->
    {flat,Sort} = set:sort(List),
    {flat,Power} = set:sets({flat,Sort}),
    {dual_ff,set:counti({flat,Power})};
canonical(List,Listl,{sides,Sides},{dimension,Dimension}) when Dimension == Listl-1 ->
    {flat,Sort} = set:sort(List),
    {flat,Power} = set:sets({flat,Sort}),
    {flat,Simplex} = set:difference({flat,Power},{flat,[]}),
    {dual_ff,set:counti({flat,Simplex})}.
orderings({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{dimension,1}) ->
    Fun0 = fun(Region) ->
        set:length(shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Region)) == 1 end,
    Start = set:choose({flat,lists:filter(Fun0,S_rs)}),
    Fun1 = fun Fun([],_,Acc) ->
        Acc; Fun(Set,Region,Acc) ->
        Boundary = set:choose(Set),
        Neighbor = neighbor(S,Boundary,Regon),
        Shell = shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Neighbor),
        Fun(set:remove(Shell,Boundary),Neighbor,[Boundary|Acc]) end,
    List = Fun1(shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Start),Start,[]),
    set:sort([List,lists:reverse(List)]).
orderings({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{dimension,S_n}) ->
    .
superspace([{dual_tf,S_dtf}|Ss_d],[{duali_tf,S_ditf}|Ss_di],
    [{boundaries_f,S_bf}|Ss_bs],[{regions_f,_}|_],[{sides_f,S_pf}|Ss_pf],
    {dimension,1}) -> %%6
    superspace_r([{dual_tf,S_dtf}|Ss_d],[{duali_tf,S_ditf}|Ss_di],
        [{boundaries_f,S_bf}|Ss_bs],[{sides_f,S_pf}|Ss_pf]); %%4
superspace([{dual_tf,S_dtf}|Ss_d],[{duali_tf,_}|_],
    [{boundaries_f,_}|_],[{regions_f,S_rf}|Ss_rs],[{sides_f,S_pf}|Ss_pf],
    {dimension,_}) ->
    superspace_r([{dual_tf,S_dtf}|Ss_d],[{regions_f,S_rf}|Ss_rs],[{sides_f,S_pf}|Ss_pf]). %%3
superspace_r([{dual_tf,S_dtf}],[{duali_tf,_}],[{boundaries_f,_}],[{sides_f,_}]) -> %%4
    dual_ff({dual_tf,S_dtf});
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],[{duali_tf,S0_ditf},{duali_tf,S1_ditf}],
    [{boundaries_f,S0_bf},{boundaries_f,S1_bf}],[{sides_f,S0_pf},{sides_f,S1_pf}]) ->
    List0 = set:choose(orderings({dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,S0_bf},{sides_f,S0_pf},{dimension,1})),
    List1 = set:choose(orderings({dual_tf,S1_dtf},{duali_tf,S1_ditf},{boundaries_f,S1_bf},{sides_f,S1_pf},{dimension,1})),
    canonical([List0,List1],2,S0_pf,{dimension,1});
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}|Ss_d],[{duali_tf,S0_ditf},{duali_tf,S1_ditf}|Ss_di],
    [{boundaries_f,S0_bf},{boundaries_f,S1_bf}|Ss_bs],[{sides_f,S0_pf},{sides_f,S1_pf}|Ss_ss]) ->
    [{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sided_f,S_pf}] = convert(
        [dual_tf,duali_tf,boundaries_f,sides_f],
        superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],[{duali_tf,S0_ditf},{duali_tf,S1_ditf}],
            [{boundaries_f,S0_bf},{boundaries_F,S1_bf}],[{sides_f,S0_pf},{sides_f,S1_pf}])), %%4
    superspace_r([{dual_tf,S_dtf}|Ss_d],[{duali_tf,S_ditf}|Ss_di],
        [{boundaries_f,S_bf}|Ss_bs],[{sides_f,S_pf},Ss_ss]). %%4
superspace_r([{dual_tf,S_dtf}],
    [{regions_f,_}],[{sides_f,_}]) -> %%3
    dual_ff({dual_tf,S_dtf});
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
    [{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}]) ->
    {flat,Shared} = set:intersect(S0_bs,S1_bs),
    {flat,Diff0} = set:difference(S0_bs,Shared),
    {flat,Diff1} = set:difference(S1_bs,Shared),
    superspace_r({flat,Shared},{dual_tf,S0_dtf},{dual_tf,S1_dtf},
        {regions_f,S0_rf},{regions_f,S1_rf},{sides_f,S0_pf},{sides_f,S1_pf},
        {flat,Diff0},{flat,Diff1},set:length(Diff0),set:length(Diff1)); %%11
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}|Ss_d],
    [{regions_f,S0_rf},{regions_f,S1_rf}|Ss_rs],[{sides_f,S0_pf},{sides_f,S1_pf}|Ss_ss]) ->
    [{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}] = convert([dual_tf,regions_f,sides_f],
        superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
            [{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}])), %%3
    superspace_r([{dual_tf,S_dtf}|Ss_d],
        [{regions_f,S_rf}|Ss_rs],[{sides_f,S_pf},Ss_ss]). %%3
superspace_r(_,{dual_tf,S0_dtf},{dual_tf,_},
    {regions_f,_},{regions_f,_},{sides_f,_},{sides_f,_},
    _,_,_,0) -> %%11
    dual_ff({dual_tf,S0_dtf});
superspace_r(_,[{dual_tf,_},{dual_tf,S1_dtf}],
    [{regions_f,_},{regions_f,_}],[{sides_f,_},{sides_f,_}],
    _,_,0,_) ->
    dual_ff({dual_tf,S1_dtf});
superspace_r({flat,Shared},[{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
    [{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}],
    {flat,Diff0},{flat,Diff1},Length0,Length1) ->
    Bound0 = set:choose({flat,Diff0}),
    Bound1 = set:choose({flat,Diff1}),
    {flat,Bounds0} = set:insert({flat,Shared},Bound0),
    {flat,Bounds1} = set:insert({flat,Shared},Bound1),
    [{side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,s2_ditf},
        {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf}] = convert(
        [side_t,dual_tf,duali_tf,boundaries_f,regions_f,sides_f],
        subspace({dual_tf,S0_dtf},{regions_f,S0_rf},{sides_f,S0_pf},{flat,Shared})),
    [{side_t,S3_st},{dual_tf,S3_dtf},{duali_tf,S3_ditf},{regions_f,S3_rf},{sides_f,S3_pf}] = convert(
        [side_t,dual_tf,duali_tf,regions_f,sides_f],
        subspace({dual_tf,S0_dtf},{regions_f,S0_rf},{sides_f,S0_pf},{flat,Bounds0})),
    [{side_t,S4_st},{dual_tf,S4_dtf},{duali_tf,S4_ditf},{regions_f,S4_rf},{sides_f,S4_pf}] = convert(
        [side_t,dual_tf,duali_tf,regions_f,sides_f],
        subspace({dual_tf,S1_dtf},{regions_f,S1_rf},{sides_f,S1_pf},{flat,Bounds1})),
    [{dual_tf,S5_dtf},{duali_tf,S5_ditf},{regions_f,S5_rf},{sides_f,S5_pf}] = convert(
        [dual_tf,duali_tf,regions_f,sides_f],
        section({side_t,S3_st},{dual_tf,S3},{duali_tf,S3_ditf},
            {regions_f,S3_rf},{sides_f,S3_pf},Bound0)),
    [{dual_tf,S6_dtf},{duali_tf,S6_ditf},{regions_f,S6_rf},{sides_f,S6_pf}] = convert(
        [dual_tf,duali_tf,regions_f,sides_f],
        section({side_t,S4_st},{dual_tf,S4_dtf},{duali,S4_ditf},
            {regions_f,S4_rf},{sides_f,S4_pf},Bound1)),
    [{dual_tf,S7_dtf},{regions_f,S7_rf},{sides_f,S7_pf}] = convert([dual_ff,regions_f,sides_f],
        supersection({side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
            {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf},
            [{dual_tf,S5_dtf},{dual_tf,S6_dtf}],[{duali_tf,S5_ditf},{duali_tf,S6_ditf}],
            [{regions_f,S5_rf},{regions_f,S6_rf}],[{sides_f,S5_pf},{sides_f,S6_pf}],
            [Bound0,Bound1])),
    [{dual_tf,S8_dtf},{regions_f,S8_rf},{sides_f,S8_pf}] = convert(
        [dual_ff,regions_f,sides_f],
        superspace_r([{dual_tf,S0_dtf},{dual_tf,S7_dtf}],
            [{regions_f,S0_rf},{regions_f,S7_rf}],[{sides_f,S0_pf},{sides_f,S7_pf}])),
    superspace_r([{dual_tf,S8_dtf},{dual_tf,S1_dtf}],
        [{regions_f,S8_rf},{regions_f,S1_rf}],[{sides_f,S8_pf},{sides_f,S1_pf}]).
