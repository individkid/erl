-module(space).

-export([side/3,side/4,side_f/4]).
-export([dual/3,dual/4,dual_ff/4,duali_ff/2]).
-export([half/3,half/4,half_ff/4,halfi/3,halfi_ff/2]).
-export([boundaries_f/1,regions_f/1,sides_f/1]).
-export([length/1,linear/1,linear/2,dimension/2]).
-export([neighbor/5,neighbor_f/5]).
-export([polyant/3,pencil/5,polycil/6]).
-export([attached/5,attached_ff/5,shell/5,shell_ff/5]).
-export([axis/7,facet/7,ordering/4]).
-export([connected/5,connected/6,canonical/4]).
-export([subspace/4,section/6,subsection/9]).
-export([supersection/8,supersecton/13,superspace/6]).

%%
%% side_f dual_ff duali_ff half_ff halfi_ff attached_ff attachedi_ff shell_ff shelli_ff
%% boundaries_f regions_f sides_f length linear dimension
%%
%% S_sf S_dff S_hff S_diff S_hiff S_aff S_bff S_aiff S_biff
%% S_bf S_rf S_pf S_m S_l S_n
%%

convert_bp({dual_ff,S_dff}) -> % helper
    {boundaries_f,S_bf} = boundaries_f({dual_ff,S_dff}),
    {sides_f,S_pf} = sides_f({dual_ff,S_ff}),
    {{boundaries_f,S_bf},{sides_f,S_pf}}.
convert_rp({dual_ff,S_dff}) ->
    {regions_f,S_rf} = regions_f({dual_ff,S_dff}),
    {sides_f,S_pf} = sides_f({dual_ff,S_ff}),
    {{regions_f,S_rf},{sides_f,S_pf}}.
convert_brp({dual_ff,S_dff}) ->
    {{boundaries_f,S_bf},{sides_f,S_pf}} = convert_bp({dual_ff,S_dff}),
    {regions_f,S_rf} = regions_f({dual_ff,S_dff}),
    {{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_drp({dual_ff,S_dff}) ->
    {dual_tf,S_dtf} = {dual_tf,set:list(set:nodet({flat,S_dff}))},
    {{regions_f,S_rf},{sides_f,S_pf}} = convert_rp({dual_ff,S_dff}),
    {{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_dbrp({dual_ff,S_dff}) ->
    {{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}} = convert_drp({dual_ff,S_dff}),
    {boundaries_f,S_bf} = boundaries_f({dual_ff,S_dff}),
    {{dual_tf,S_dtf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_di({dual_ff,S_dff}) ->
    {dual_tf,S_dtf} = {dual_tf,set:list(set:nodet({flat,S_dff}))},
    {duali_ff,S_diff} = duali_ff({dual_ff,S_dff}),
    {duali_tf,S_ditf} = {duali_tf,set:list(set:nodet({flat,S_diff}))},
    {{dual_tf,S_dtf},{duali_tf,S_ditf}}.
convert_dirp({dual_ff,S_dff}) ->
    {{dual_tf,S_dtf},{duali_tf,S_ditf}} = convert_di({dual_ff,S_dff}),
    {{regions_f,S_rf},{sides_f,S_pf}} = convert_rp({dual_ff,S_dff}),
    {{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_dibp({dual_ff,S_dff}) ->
    {{dual_tf,S_dtf},{duali_tf,S_ditf}} = convert_di({dual_ff,S_dff}),
    {{boundaries_f,S_bf},{sides_f,S_pf}} = convert_bp({dual_ff,S_dff}),
    {{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf}}.
convert_dibrp({dual_ff,S_dff}) ->
    {{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf}} = convert_dirp({dual_ff,S_dff}),
    {boundaries_f,S_bf} = boundaries_f({dual_ff,S_dff}),
    {{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_sdbrp({dual_ff,S_dff}) ->
    {{dual_tf,S_dtf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}} = convert_dbrp({dual_ff,S_dff}),
    {dual_tt,S_dtt} = {dual_tt,set:list(set:leaft({tree,S_dtf}))},
    {side_f,S_sf} = side_f({dual_tt,S_dtt},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}),
    {side_t,S_st} = {side_t,set:list(set:nodet({flat,S_sf}))},
    {{side_t,S_st},{dual_tf,S_dtf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_sdibrp({dual_ff,S_dff}) ->
    {{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}} = convert_dibrp({dual_ff,S_dff}),
    {dual_tt,S_dtt} = {dual_tt,set:list(set:leaft({tree,S_dtf}))},
    {side_f,S_sf} = side_f({dual_tt,S_dtt},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}),
    {side_t,S_st} = {side_t,set:list(set:nodet({flat,S_sf}))},
    {{side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.

convert_sfbrp({side_f,S_sf}) -> % helper
    {side_t,S_st} = {side_t,set:list(set:nodet({flat,S_sf}))},
    {boundaries_f,S_bf} = boundaries_f({side_f,S_sf}),
    {regions_f,S_rf} = regions_f({side_f,S_sf}),
    {sides_f,S_pf} = sides_f({side_f,S_sf}),
    {dual_ff,S_dff} = dual_ff({side_t,Tab_t},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}),
    {{side_t,S_st},{dual_ff,S_dff},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_sdbrp({side_f,S_sf}) ->    
    {{side_t,S_st},{dual_ff,S_dff},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}} = convert_sfbrp({side_f,S_sf}),
    {dual_tf,S_dtf} = {dual_tf,set:list(set:nodet({flat,S_dff}))},
    {{side_t,S_st},{dual_tf,S_dtf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.
convert_sdibrp({side_f,S_sf}) ->    
    {{side_t,S_st},{dual_ff,S_dff},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}} = convert_sfbrp({side_f,S_sf}),
    {{dual_tf,S_dtf},{duali_tf,S_ditf}} = convert_di({dual_ff,S_dff}),
    {{side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}}.

side({dual_tt,Tab_t},{sides_f,Sides},Boundary,Region) -> % inducer
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
side_f({dual_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) -> % converter
    Fun = fun(Boundary,Region) ->
        side({dual_tt,Tab_t},{sides_f,Sides},Boundary,Region) end,
    {side_f,lambda:tabulate(Boundaries,Regions,Fun)};
side_f({half_tt,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) ->
    Fun = fun(Boundary,Region) ->
        side({half_tt,Tab_t},{sides_f,Sides},Boundary,Region) end,
    {side_f,lambda:tabulate(Boundaries,Regions,Fun)}.
side_b({side_t,Tab_t},Side,Boundary) -> % helper
    fun(Region) ->
        set:access({side_t,Tab_t},Boundary,Region) == Side end.
side_r({side_t,Tab_t},Side,Region) -> % helper
    fun(Boundary) ->
        set:access({side_t,Tab_t},Boundary,Region) == Side end.

dual({side_t,Tab_t},{boundaries_f,Boundaries},Side,Region) -> % inducer
    Fun = side_r({side_t,Tab_t},Side,Region),
    {flat,lists:filter(Fun,Boundaries)}.
dual_ff({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) -> % converter
    Fun = fun(Side,Region) ->
        dual({side_t,Tab_t},{boundaries_f,Boundaries},Side,Region) end,
    {dual_ff,lambda:tabulate(Sides,Regions,Fun)}.
duali_ff({dual_tf,Tab_t},{sides_f,Sides}) -> % converter
    Fun = fun(Side) ->
        set:inverse(set:get({tree,Tab_t},Side)) end,
    {duali_ff,lambda:tabulate(Sides,Fun)}.

half({side_t,Tab_t},{regions_f,Regions},Side,Boundary) -> % inducer
    Fun = side_b({side_t,Tab_t},Side,Boundary),
    {flat,lists:filter(Fun,Regions)}.
half_ff({side_t,Tab_t},{boundaries_f,Boundaries},{regions_f,Regions},{sides_f,Sides}) -> % converter
    Fun = fun(Side,Boundary) ->
        half({side_t,Tab_t},{side_f,Regions},Side,Boundary) end,
    {half_ff,lambda:tabulate(Sides,Boundaries,Fun)}.
halfi_ff({half_tf,Tab_t},{sides_f,Sides}) -> % converter
    Fun = fun(Side) ->
        set:inverse(set:flat(set:get({tree,Tab_t},Side))) end,
    {halfi_ff,lambda:tabulate(Sides,Fun)}.

boundaries_f({side_f,Tab_f}) -> % converter
    {boundaries_f,set:list(set:domain({flat,Tab_f}))};
boundaries_f({dual_ff,Tab_f}) -> % converter
    {boundaries_f,set:list(set:range(set:unionf(set:range({flat,Tab_f}))))};
boundaries_f({half_ff,Tab_f}) -> % converter
    {boundaries_f,set:list(set:domain(set:unionf(set:range({flat,Tab_f}))))}.
regions_f({side_f,Tab_f}) -> % converter
    {regions_f,set:list(set:domain(set:unionf(set:range({flat,Tab_f}))))};
regions_f({dual_ff,Tab_f}) -> % converter
    {regions_f,set:list(set:domain(set:unionf(set:range({flat,Tab_f}))))};
regions_f({half_ff,Tab_f}) -> % converter
    {regions_f,set:list(set:range(set:unionf(set:range({flat,Tab_f}))))}.
sides_f({side_f,Tab_f}) -> % converter
    {sides_f,set:list(set:range(set:unionf(set:range({flat,Tab_f}))))};
sides_f({dual_ff,Tab_f}) -> % converter
    {sides_f,set:list(set:domain({flat,Tab_f}))};
sides_f({half_ff,Tab_f}) -> % converter
    {sides_f,set:list(set:domain({flat,Tab_f}))}.

length({boundaries_f,Tab_f}) -> % converter
    {length,set:length({flat,Tab_f})}.
linear({regions_f,Tab_f}) -> % converter
    {linear,set:length({flat,Tab_f})}.
linear({dimension,_},{length,0}) -> % converter
    {linear,1};
linear({dimension,0},{length,_}) ->
    {linear,1};
linear({dimension,Dim},{length,Len}) ->
    {linear,Left} = linear({dimension,Dim},{length,Len-1}),
    {linear,Right} = linear({dimension,Dim-1},{length,Len-1}),
    {linear,Left+Right}.
dimension({length,Len},{linear,Lin}) -> % converter
    fun (true) ->
        {dimension,0};
    (false) ->
        {dimension,dimension_r(Len,Lin,1)} end
    (linear({dimension,0},{length,Len}) == {linear,Lin}).
dimension_r(Len,Lin,Dim) -> % recursor
    fun (true) ->
        Dim;
    (false) ->
        fun (true) ->
            dimension_r(Len,Lin,Dim,Dim div 2);
        (false) ->
            dimension_r(Len,Lin,Dim * 2) end
        (linear({dimension,Dim * 2},{length,Len}) > {linear,Lin}) end
    (linear({dimension,Dim},{length,Len}) == {linear,Lin}).
dimension_r(Len,Lin,Dim,Amt) -> % recursor
    fun (true) ->
        Dim+Amt;
    (false) ->
        fun (true) ->
            dimension_r(Len,Lin,Dim,Amt div 2);
        (false) ->
            dimension_r(Len,Lin,Dim+Amt,Amt div 2) end
        (linear({dimension,Dim+Amt},{length,Len}) > {linear,Lin}) end
    (linear({dimension,Dim+Amt},{length,Len}) == {linear,Lin}).

neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{flat,Boundaries},Region) -> % deducer
    % region
    Side = set:choose({flat,S_pf}),
    {flat,Set} = {flat,set:access({dual_tf,S_dtf},Side,Region)},
    {flat,Sym} = set:symmetric({flat,Boundaries},{flat,Set}),
    set:access({duali_tf,S_ditf},Side,{flat,Sym});
neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Boundary,Region) -> % inducer
    % region
    {flat,Set} = set:singleton(Boundary),
    neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{flat,Set},Region).
neighbor_f({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) -> % converter
    Fun = fun(Boundary,Region) ->
        neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Boundary,Region) end,
    {neighbor_f,lambda:tabulate(S_bf,S_rf,Fun)}.
neighbor_b({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Boundary) -> % helper
    % fun from region to bool
    fun(Region) ->
        try neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Boundary,Region) of _ ->
            true catch _ ->
            false end end.
neighbor_r({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Region) -> % helper
    % fun from boundary to bool
    fun(Boundary) ->
        try neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Boundary,Region) of _ ->
            true catch _ ->
            false end end.
polyant({side_t,S_st},{regions_f,S_rf},{flat,Map}) -> % deducer
    % quadrant? flat regions
    Fun = fun(Region) ->
        Fun = fun({Boundary,Side}) ->
            set:access({side_t,S_st},Boundary,Region) /= Side end,
        not lists:any(Fun,Map) end,
    {flat,lists:filter(Fun,S_rf)}.
pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,Boundaries}) -> % deducer
    % flat regions
    Fun = fun(Region) ->
        try neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{flat,Boundaries},Region) of _ ->
            true catch _ ->
            false end end,
    {flat,lists:filter(Fun,S_rf)}.
polycil({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},{flat,Map}) -> % deducer
    % flat regions
    {flat,Sup} = polyant({side_t,S_st},{regions_f,S_rf},{flat,Map}),
    {flat,Boundaries} = set:domain({flat,Map}),
    {flat,Sub} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,Boundaries}),
    set:intersect({flat,Sup},{flat,Sub}).

attached({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},Boundary) -> % inducer
    Fun = neighbor_b({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
        Boundary),
    {flat,lists:filter(Fun,S_rf)}.
attached_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) -> % converter
    Fun = fun(Boundary) ->
        set:list(attached({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},Boundary)) end,
    {attached_ff,lambda:tabulate(S_bf,Fun)}.
shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Region) -> % inducer
    Fun = neighbor_r({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},
        Region),
    {flat,lists:filter(Fun,S_bf)}.
shell_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}) -> % converter
    Fun = fun(Region) ->
        shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},Region) end,
    {shell_ff,lambda:tabulate(S_rf,Fun)}.

axis({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,I0},{flat,I1}) -> % deducer
    % one to one flat map from pencil of incidence I0 to pencil of I1
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
    {flat,P0} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,I0}),
    {flat,P1} = pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,I1}),
    R0 = set:choose({flat,P0}),
    Wrap = fun(Region,Boundary) ->
        set:access({side_t,S_st},Boundary,Region) end,
    Conv0 = fun({B0,{B1,Side}}) ->
        {B0,Xor(Side,(set:access({side_t,S_st},B0,R0)!=set:access({side_t,S_st},B1,R0)))} end,
    Conv1 = fun(R1,{flat,Map}) ->
        {R1,lists:map(Conv0,lambda:zip(I0,Map))} end,
    {tree,Map0} = set:tree({flat,lambda:tabulate(P0,I0,Wrap)}),
    {flat,Map} = {flat,lambda:tabulate(P1,I1,Wrap)},
    {tree,Map1} = set:tree(set:inverse({flat,lists:map(Conv1,Map)})),
    Fun = fun(Region) ->
        Map = set:get({tree,Map0},Region),
        set:get({tree,Map1},Map) end,
    {flat,lists:map(Fun,P0)}.
facet({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},Boundary,Region) -> % deducer
    % shell in section by boundary
    {{dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,S0_bf},{sides_f,S0_pf}} = convert_dibp(
        section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,[Boundary]})),
    Side = set:choose({flat,S_pf}),
    {flat,Dual} = {flat,set:access({dual_tf,S_dtf},Side,Region)},
    {flat,Diff} = set:difference({flat,Dual},set:singleton(Boundary)),
    Take = set:access({duali_tf,S0_ditf},Side,{flat,Diff})},
    shell({dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,S0_bf},{sides_f,S0_pf},Take).
ordering({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf}) -> % deducer
    % only for 1 dimension
    Fun0 = fun(Region) -> % TODO use shell_ff
        {Region,shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Region)}
    Fun1 = fun({_,{flat,Set}}) ->
        set:length({flat,Set}) == 1 end,
    {Region,{flat,[Boundary]}} = set:choose({flat,lists:filter(Fun1,lists:map(Fun0,S_rf))}),
    fun Fun([],_,Acc) ->
        Acc; Fun(Set,Region,Acc) ->
        Boundary = set:choose(Set),
        Neighbor = neighbor(S,Boundary,Region), % TODO
        Shell = shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Neighbor),
        Fun(set:remove(Shell,Boundary),Neighbor,[Boundary|Acc]) end
    (shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Start),Start,[]).
connected({dual_tf,_},{duali_tf,_},{boundaries_f,_},{sides_f,_},{flat,[]}) -> % deducer
    % flat regions
    {flat,[]};
connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Regions}) ->
    connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},
        {flat,Regions},set:choose({flat,Regions})).
connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Regions},Region) -> % deducer
    set:sort(connected_r({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Regions},[],[])).
connected_r({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Regions},[],Acc) -> % recursor
    Acc;
connected_r({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Regions},[H|T],Acc) ->
    {flat,Shell} = shell({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},H),
    {flat,Todo} = set:intersect({flat,Shell},{flat,Regions}),
    {flat,Diff} = set:difference({flat,Regions},{flat,Todo}),
    connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},Diff,T++Todo,[H|Acc]).
canonical(List,Listl,{sides_f,Sides},{dimension,1}) -> % constructor
    Side = set:choose(Sides),
    Fun0 = fun Fun(Head,Tail,0) ->
        Head;
    (Head,{H|T},Num) ->
        Fun([H|Head],T,Num-1) end,
    Fun1 = fun(S,Region) when S == Side ->
        lists:reverse(Fun0([],List,Region));
    fun(_,Region) ->
        lists:nthtail(List,Region) end,
    {dual_ff,lambda:tabulate(Sides,lists:seq(0,Listl-1),Fun1)};
canonical(List,Listl,{sides,Sides},{dimension,Dimension}) when Dimension == Listl ->
    {flat,Sort} = set:sort(List),
    {flat,Power} = set:sets({flat,Sort}),
    {dual_ff,set:counti({flat,Power})};
canonical(List,Listl,{sides,Sides},{dimension,Dimension}) when Dimension == Listl-1 ->
    {flat,Sort} = set:sort(List),
    {flat,Power} = set:sets({flat,Sort}),
    {flat,Simplex} = set:difference({flat,Power},{flat,[]}),
    {dual_ff,set:counti({flat,Simplex})}.
subspace({dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},{flat,Set}) -> % constructor
    Fun0 = fun(Region,Side) ->
        {flat,Dual} = {flat,set:access({tree,S_dtf},Side,Region)},
        set:intersect({flat,Set},{flat,Dual}) end,
    {flat,Map0} = set:uniquefyv(set:tabulate(S_rf,S_pf,Fun0)),
    {tree,Map1} = set:nodet({flat,Map0}),
    Fun1 = fun(Side,Region) ->
        set:get(set:get({tree,Map1},Region),Side) end,
    {dual_ff,lambda:tabulate(S_pf,set:list(set:domain({flat,Map0})),Fun1)}.
section({side_t,_},{dual_tf,S_dtf},{duali_tf,_},{regions_f,_},{sides_f,_},{flat,[]}) -> % constructor
    {dual_ff,set:list(set:flat({dual_tf,S_dtf}))};
section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,[Boundary]}) ->
    Side = set:choose(S_pf),
    {tree,Map} = {tree,set:access({side_t,S_st},Boundary)},
    Fun0 = fun(Region) ->
        set:get({tree,Map},Region) == Side end,
    {flat,List} = attached({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},Boundary),
    {flat,Attached} = {flat,lists:filter(Fun0,List)},
    Fun1 = fun(Side,Region) ->
        {flat,Dual} = set:access({dual_tf,S_dtf},Side,Region),
        set:remove({flat,Dual},Boundary) end,
    {dual_ff,lambda:tabulate(S_pf,Attached,Fun1)};
section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,[Boundary|Boundaries]}) ->
    [{side_t,S0_st},{dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,_},{regions_f,S0_rf},{sides_f,S0_pf}] = convert_sdibrp(
        section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,[Boundary]})),
    section({side_t,S0_st},{dual_tf,S0_dtf},{duali_tf,S0_ditf},{regions_f,S0_rf},{sides_f,S0_pf},{flat,Boundaries}).
%% sections have same boundaries as space
subsection({side_t,S_st},{dual_tf,_},{duali_tf,_},{boundaries_f,_},{regions_f,_},{sides_f,_},
    [],[],[]) -> % constructor
    {side_f,set:list(set:nodef({side_t,S_st}))};
subsection({side_t,_},{dual_tf,_},{duali_tf,_},{boundaries_f,_},{regions_f,_},{sides_f,_},
    [{dual_tf,S0_dtf}],[{regions_f,S0_rf}],[{sides_f,S0_pf}]) ->
    {dual_ff,S0_dff} = {dual_ff,set:list(set:nodef({dual_tf,S0_dtf}))},
    {boundaries_f,S0_bf} = boundaries_f({dual_ff,S0_dff}),
    {dual_tt,S0_dtt} = {dual_tt,set:list(set:leaft({dual_tf,S0_dtf}))},
    side_f({dual_tf,S0_dtf},{boundaries_f,S0_bf},{regions_f,S0_rf},{sides_f,S0_pf});
subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{dual_tf,S0_dtf},{dual_tf,S1_dtf}],[{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}]) ->
    Boundary = set:choose({flat,S_bf}),
    {flat,Boundaries} = set:remove({flat,S_bf},Boundary),
    {{side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
        {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf}} = convert_sdibrp(
        subspace({dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},{flat,Boundaries})),
    {{dual_tf,S3_dtf},{regions_f,S3_rf},{sides_f,S3_pf}} = convert_drp(
        subspace({dual_tf,S0_dtf},{regions_f,S0_rf},{sides_f,S0_pf},{flat,Boundaries})),
    {{dual_tf,S4_dtf},{regions_f,S4_rf},{sides_f,S4_pf}} = convert_drp(
        subspace({dual_tf,S1_dtf},{regions_f,S1_rf},{sides_f,S1_pf},{flat,Boundaries})),
    {{dual_tf,S5_dtf},{regions_f,S5_rf},{sides_f,S5_pf}} = convert_drp(
        section({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,[Boundary]})),
    {{side_t,S6_st},{dual_tf,S6_dtf},{duali_tf,S6_ditf},
        {boundaries_f,S6_bf},{regions_f,S6_rf},{sides_f,S6_pf}} = convert_sdibrp(
        subsection({side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
            {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf},
            [{dual_tf,S3_dtf},{dual_tf,S4_dtf}],
            [{regions_f,S3_rf},{regions_f,S4_rf}],
            [{sides_f,S3_pf},{sides_f,S4_pf}])),
    {{side_t,_},{dual_tf,_},{duali_tf,S7}
        {boundaries_f,_},{regions_f,_},{sides_f,_}} = convert_sdibrp(
        subsection({side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
            {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf},
            [{dual_tf,S5_dtf},{dual_tf,S6_dtf}],
            [{regions_f,S5_rf},{regions_f,S6_rf}],
            [{sides_f,S5_pf},{sides_f,S6_pf}])),
    supersection({side_t,S6_st},{dual_tf,S6_dtf},{duali_tf,S6_ditf},
        {boundaries_f,S6_bf},{regions_f,S6_rf},{sides_f,S6_pf},
        {duali_tf,S7_ditf},Boundary);
subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{dual_tf,S0_dtf}|S0s_d],[{regions_f,S0_rf}|S0s_rp],[{sides_f,S0_pf}|S0s_pf]) ->
    Fun = fun({{dual_tf,S1_dtf},{regions_f,S1_rf},{sides_f,S1_pf}}) ->
        subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
            {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
            [{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
            [{regions_f,S0_rf},{regions_f,S1_rf}],
            [{sides_f,S0_pf},{sides_f,S1_pf}]) end,
    S1s_s = lists:map(Fun,lambda:zip(S0s_d,S0s_rp,S0s_pf)),
    subsection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
        {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        lists:map(fun(S1_s) -> convert(dual_tf,S1_s) end,S1s_s),
        lists:map(fun(S1_s) -> convert(regions_f,S1_s) end,S1s_s),
        lists:map(fun(S1_s) -> convert(sides_f,S1_s) end,S1s_s)).
%% sections have same boundaries as space
supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    {duali_tf,S0_ditf},Boundary) -> % constructor
    {flat,Boundaries} = set:insert(S_bf,Boundary),
    Side = set:choose({flat,S_pf}),
    Other = set:choose(set:remove({flat,S_pf},Side)),
    {tree,Sidedness} = set:tree(set:unionk(set:singleton({false,Side}),set:singleton({true,Other}))),
    {tree,Truth} = set:tree(set:inverse({flat,Sidedness})),
    Fun0 = fun(Region) ->
        {flat,Set} = {flat,set:access({dual_t,S_dtf},Side,Region)},
        {tree,Map} = {tree,set:access({duali_tf,S0_ditf},Side)},
        set:memberk({tree,Map},{flat,Set}) end,
    {flat,Divided} = {flat,lists:filter(Fun0,S_rf)},
    {flat,Diff} = set:difference({flat,S_rf},{flat,Divided}),
    {tree,Half} = set:tree(connected({dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf},{flat,Diff})),
    Fun1 = fun(Bound,Region) when Bound == Boundary ->
        set:get({tree,Sidedness},set:member({tree,Half},Region)); (_,_) ->
        set:access({side_t,S_st},Bound,Region) end,
    {flat,Ground} = {flat,lambda:tabulate(Boundaries,S_rf,Fun1)}, % regions on Side of Boundary if in Half
    Fun2 = fun(Bound,Region) when Bound == Boundary ->
        Side = set:get(set:get({flat,Ground},Bound),Region), % TODO
        set:get({tree,Sidedness},not set:get({tree,Truth},Side)); (Bound,Region) ->
        set:get(set:get({flat,Ground},Bound),Region) end, % TODO
    % Divided to regions similar except opposite Boundary
    {tree,Figure} = set:nodet({flat,lambda:tabulate(Boundaries,Divided,Fun2)}),
    {tree,Map} = set:tree(set:sort(lambda:zip(Divided,set:holes({flat,S_rf},set:length({flat,Divided}))))),
    Fun3 = fun({Region,Side}) ->
        {set:get({tree,Map},Region),Side} end,
    Fun4 = fun(Bound) ->
        set:sort(lists:map(Fun3,set:get({tree,Figure},Bound))) end,
    % return union of augmented regions and complementary divided
    {side_f,set:list(set:unionk({flat,Ground},{flat,lambda:tabulate(Boundaries,Fun4)}))};
supersection({side_t,S_st},{dual_tf,_},{duali_tf,_},{boundaries_f,_},{regions_f,_},{sides_f,_},
    [],[],[],[],[],[],[]) ->
    {side_f,set:list(set:nodef({side_t,S_st}))};
supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{side_t,S0_st}],[{dual_tf,S0_dtf}],[{duali_tf,S0_ditf}],[{boundaries_f,S0_bf}],[{regions_f,S0_rf}],[{sides_f,S0_pf}],
    [Boundary]) ->
    supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
        {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        {duali_tf,S0_ditf},Boundary);
supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
    [{side_t,S0_st}|S0s_s],[{dual_tf,S0_dtf}|S0s_d],[{duali_tf,S0_ditf}|S0s_di],
    [{boundaries_f,S0_bf}|S0s_bf],[{regions_f,S0_rf}|S0s_rf],[{sides_f,S0_pf}|S0s_pf],
    [Boundary|Boundaries]) ->
    Fun = fun({{side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
        {boundaries_f,S1_bf},{regions_rf,S1_rf},{sides_pf,S1_pf}}) ->
        {duali_tf,S2_ditf} = {duali_tf,set:list(set:nodet(subsection(
                        {side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
                        {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
                        [{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
                        [{regions_f,S0_rf},{regions_f,S1_rf}],
                        [{sides_f,S0_pf},{sides_f,S1_pf}])))},
        supersection({side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
            {boundaries_f,S1_bf},{regions_f,S1_rf},{sides_f,S1_pf},
            {duali_tf,S2_ditf},Boundary) end,
    S1s = lists:map(fun(S0) -> convert_sdibrf(S0) end,
        lists:map(Fun,lambda:zip(S0s_s,S0s_d,S0s_di,S0s_bf,S0s_rf,S0s_pf))),
    {{side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
        {boundaries_f,S1_bf},{regions_f,S1_rf},{sides_f,S1_pf}} = convert_sdibrp(
        supersection({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},
            {boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
            {duali_tf,S0_ditf},Boundary)),
    supersection({side_t,S1_st},{dual_tf,S1_dtf},{duali_tf,S1_ditf},
        {boundaries_f,S1_bf},{regions_f,S1_rf},{sides_f,S1_rf},
        lists:map(fun(S1) -> erlang:element(1,S1) end,S1s),
        lists:map(fun(S1) -> erlang:element(2,S1) end,S1s),
        lists:map(fun(S1) -> erlang:element(3,S1) end,S1s),
        lists:map(fun(S1) -> erlang:element(4,S1) end,S1s),
        lists:map(fun(S1) -> erlang:element(5,S1) end,S1s),
        lists:map(fun(S1) -> erlang:element(6,S1) end,S1s),
        Boundaries).
superspace([{dual_tf,S_dtf}|Ss_d],[{duali_tf,S_ditf}|Ss_di],
    [{boundaries_f,S_bf}|Ss_bf],[{regions_f,_}|_],[{sides_f,S_pf}|Ss_pf],
    {dimension,1}) -> % constructor % 6
    superspace_r([{dual_tf,S_dtf}|Ss_d],[{duali_tf,S_ditf}|Ss_di],
        [{boundaries_f,S_bf}|Ss_bf],[{sides_f,S_pf}|Ss_pf]); % 4
superspace([{dual_tf,S_dtf}|Ss_d],[{duali_tf,_}|_],
    [{boundaries_f,_}|_],[{regions_f,S_rf}|Ss_rf],[{sides_f,S_pf}|Ss_pf],
    {dimension,_}) ->
    superspace_r([{dual_tf,S_dtf}|Ss_d],[{regions_f,S_rf}|Ss_rf],[{sides_f,S_pf}|Ss_pf]). % 3
superspace_r([{dual_tf,S_dtf}],[{duali_tf,_}],[{boundaries_f,_}],[{sides_f,_}]) -> % 4
    dual_ff({dual_tf,S_dtf});
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],[{duali_tf,S0_ditf},{duali_tf,S1_ditf}],
    [{boundaries_f,S0_bf},{boundaries_f,S1_bf}],[{sides_f,S0_pf},{sides_f,S1_pf}]) ->
    List0 = ordering({dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,S0_bf},{sides_f,S0_pf}),
    List1 = ordering({dual_tf,S1_dtf},{duali_tf,S1_ditf},{boundaries_f,S1_bf},{sides_f,S1_pf}),
    canonical([List0,List1],2,S0_pf,{dimension,1});
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}|Ss_d],[{duali_tf,S0_ditf},{duali_tf,S1_ditf}|Ss_di],
    [{boundaries_f,S0_bf},{boundaries_f,S1_bf}|Ss_bf],[{sides_f,S0_pf},{sides_f,S1_pf}|Ss_ss]) ->
    {{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,S_bf},{sides_f,S_pf}} = convert_dibp(
        superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],[{duali_tf,S0_ditf},{duali_tf,S1_ditf}],
            [{boundaries_f,S0_bf},{boundaries_f,S1_bf}],[{sides_f,S0_pf},{sides_f,S1_pf}])), % 4
    superspace_r([{dual_tf,S_dtf}|Ss_d],[{duali_tf,S_ditf}|Ss_di],
        [{boundaries_f,S_bf}|Ss_bf],[{sides_f,S_pf},Ss_ss]). % 4
superspace_r([{dual_tf,S_dtf}],
    [{regions_f,_}],[{sides_f,_}]) -> % 3
    dual_ff({dual_tf,S_dtf});
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
    [{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}]) ->
    {flat,Shared} = set:intersect(S0_bf,S1_bf),
    {flat,Diff0} = set:difference(S0_bf,Shared),
    {flat,Diff1} = set:difference(S1_bf,Shared),
    superspace_r({flat,Shared},{dual_tf,S0_dtf},{dual_tf,S1_dtf},
        {regions_f,S0_rf},{regions_f,S1_rf},{sides_f,S0_pf},{sides_f,S1_pf},
        {flat,Diff0},{flat,Diff1},set:length(Diff0),set:length(Diff1)); % 11
superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}|Ss_d],
    [{regions_f,S0_rf},{regions_f,S1_rf}|Ss_rf],[{sides_f,S0_pf},{sides_f,S1_pf}|Ss_ss]) ->
    {{dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf}} = convert_drp(
        superspace_r([{dual_tf,S0_dtf},{dual_tf,S1_dtf}],
            [{regions_f,S0_rf},{regions_f,S1_rf}],[{sides_f,S0_pf},{sides_f,S1_pf}])), % 3
    superspace_r([{dual_tf,S_dtf}|Ss_d],
        [{regions_f,S_rf}|Ss_rf],[{sides_f,S_pf},Ss_ss]). % 3
superspace_r(_,{dual_tf,S0_dtf},{dual_tf,_},
    {regions_f,_},{regions_f,_},{sides_f,_},{sides_f,_},
    _,_,_,0) -> % 11
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
    {{side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,s2_ditf},
        {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf}} = convert_sdibrp(
        subspace({dual_tf,S0_dtf},{regions_f,S0_rf},{sides_f,S0_pf},{flat,Shared})),
    {{side_t,S3_st},{dual_tf,S3_dtf},{duali_tf,S3_ditf},
        {boundaries_f,_},{regions_f,S3_rf},{sides_f,S3_pf}} = convert_sdibrp(
        subspace({dual_tf,S0_dtf},{regions_f,S0_rf},{sides_f,S0_pf},{flat,Bounds0})),
    {{side_t,S4_st},{dual_tf,S4_dtf},{duali_tf,S4_ditf},
        {boundaries_f,_},{regions_f,S4_rf},{sides_f,S4_pf}} = convert_sdibrp(
        subspace({dual_tf,S1_dtf},{regions_f,S1_rf},{sides_f,S1_pf},{flat,Bounds1})),
    {{dual_tf,S5_dtf},{duali_tf,S5_ditf},{regions_f,S5_rf},{sides_f,S5_pf}} = convert_dirp(
        section({side_t,S3_st},{dual_tf,S3},{duali_tf,S3_ditf},{regions_f,S3_rf},{sides_f,S3_pf},Bound0)),
    {{dual_tf,S6_dtf},{duali_tf,S6_ditf},{regions_f,S6_rf},{sides_f,S6_pf}} = convert_dirp(
        section({side_t,S4_st},{dual_tf,S4_dtf},{duali,S4_ditf},{regions_f,S4_rf},{sides_f,S4_pf},Bound1)),
    {{dual_tf,S7_dtf},{regions_f,S7_rf},{sides_f,S7_pf}} = convert_drp(
        supersection({side_t,S2_st},{dual_tf,S2_dtf},{duali_tf,S2_ditf},
            {boundaries_f,S2_bf},{regions_f,S2_rf},{sides_f,S2_pf},
            [{dual_tf,S5_dtf},{dual_tf,S6_dtf}],[{duali_tf,S5_ditf},{duali_tf,S6_ditf}],
            [{regions_f,S5_rf},{regions_f,S6_rf}],[{sides_f,S5_pf},{sides_f,S6_pf}],
            [Bound0,Bound1])),
    {{dual_tf,S8_dtf},{regions_f,S8_rf},{sides_f,S8_pf}} = convert_drp(
        superspace_r([{dual_tf,S0_dtf},{dual_tf,S7_dtf}],
            [{regions_f,S0_rf},{regions_f,S7_rf}],[{sides_f,S0_pf},{sides_f,S7_pf}])),
    superspace_r([{dual_tf,S8_dtf},{dual_tf,S1_dtf}],
        [{regions_f,S8_rf},{regions_f,S1_rf}],[{sides_f,S8_pf},{sides_f,S1_pf}]).
