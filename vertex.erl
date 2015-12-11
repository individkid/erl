-module(vertex).
-export([convert/2,inside/4,outside/4]).
-export([incidences/2,incidences/6,vertex/2,vertexi/2]).
-export([corners/2,corners/3,neighbors/2]).
-export([partial/3,partial_f/4,partialb/2,partialr/2,partialv/2]).
-export([prior/4,sign/6,cosection/4,cospace/2,roundspace/7]).
-export([signifv/8,signifb/8,signifr/8]).
-export([signifv_f/8,signifb_ff/7,signifr_ff/7]).
-export([polytope/10,embed/1]).

%%
%% inside_f outside_f incidences_ff vertex_ff vertexi_f
%% vertices_f corners_ff neighbors_ff
%% partial_f partialb_ff partialr_ff partialv_ff
%% signifv_f signifb_ff signifr_ff
%%
%% S_if S_of S_iff S_vf S_vif
%% S_jf S_cff S_nff
%% S_of S_obff S_orff S_ovff
%% S_gvf S_gbff S_grff
%%

convert_sdibrpqi({dual_ff,S_dff},{dimension,S_n}) ->
    {dual_tf,S_dtf} = {dual_tf,set:list(set:nodet({flat,S_dff}))},
    {duali_ff,S_diff} = space:duali_ff({dual_ff,S_dff}),
    {duali_tf,S_ditf} = {duali_tf,set:list(set:nodet({flat,S_diff}))},
    {boundaries_f,S_bf} = space:boundaries_f({dual_ff,S_dff}),
    {regions_f,S_rf} = space:regions_f({dual_ff,S_dff}),
    {sides_f,S_pf} = space:sides_f({dual_ff,S_dff}),
    {dual_tt,S_tt} = {dual_tt,set:list(set:leaft({tree,S_tf}))},
    {side_f,S_sf} = space:side_f({dual_tt,S_dtt},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf}),
    {side_t,S_st} = {side_t,set:list(set:nodet({flat,S_sf}))},
    {incidences_ff,S_qff} = incidences_ff({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},
        {regions_f,S_rf},{sides_f,S_pf},{dimension,Dim}),
    {incidences_tf,S_qtf} = {incidences_tf,set:list(set:nodet({flat,S_iff}))},
    {vertex_ff,S_vff} = vertex_ff({incidences_tf,S_qtf},{regions_f,S_rf}),
    {vertexi_ff,S_viff} = {vertexi_ff,set:list(set:inverse({flat,S_vff}))},
    {vertexi_tf,S_vitf} = {vertexi_tf,set:list(set:nodet({flat,S_viff}))},
    {{side_t,S_st},{dual_t,S_dt},{duali_t,S_dit},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},
        {incidences_tf,S_qtf},{vertexi_tf,S_vitf}}.

inside({side_t,S_t},{half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region) -> % inducer
    {flat,Shell} = space:shell({shell_tf,S_btf},Region),
    {flat,Sets} = set:sets({flat,Shell},Dim),
    Fun0 = fun({flat,Set}) ->
        Fun = fun(Boundary) ->
            set:list(space:half({half_tf,S_htf},set:access({side_t,S_t},Boundary,Region),Boundary)) end,
        set:list(set:intersectf(set:sort(lists:map(Fun,Set)))) end,
    Fun1 = fun(List) ->
        erlang:length(List) == 0 end,
    lists:any(Fun1,lists:map(Fun0,Sets)).
outside({side_t,S_t},{half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region) -> % inducer
    not inside({half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region).
inside_f({half_tf,S_htf},{shell_tf,S_btf},{regions_f,S_rf},{dimension,Dim}) -> % converter
    Fun = fun(Region) ->
        inside({half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region) end,
    {inside_f,lists:filter(Fun,S_rf)}.
outside_f({half_tf,S_htf},{shell_tf,S_btf},{regions_f,S_rf},{dimension,Dim}) -> % converter
    Fun = fun(Region) ->
        outside({half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region) end,
    {outside_f,lists:filter(Fun,S_rf)}.
incidences({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{dimension,Dim},Region) -> % inducer
    {flat,Power} = set:sets(shell({shell_tf,S_btf},Region)),
    Fun = fun(List) ->
        erlang:length(List) >= Dim andalso
        try space:neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{flat,Filter},Region) of _ ->
            true catch _ ->
            false end end,
    {flat,lists:filter(Fun,Power)}.
incidences_ff({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{dimension,Dim}) ->
    % converter
    Fun = fun(Region) ->
        set:list(incidences({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{dimension,Dim},Region)) end,
    {incidences_ff,lambda:tabulate(S_rf)}.
vertex_ff({incidences_tf,S_itf},{regions_f,S_rf}) -> % converter
    Fun = fun(Region) ->
        set:list(incidences({incidences_tf,S_vt},Region)) end,
    {vertex_ff,set:count(lists:append(lists:map(Fun,S_rf)))}.
corners({incidences_tf,S_itf},{vertex_tf,S_vtf},Region) -> % inducer
    Fun = fun(List) ->
        set:get({tree,S_vtf},List) end,
    Incidences = incidences({incidences_tf,S_itf},Region),
    set:sort(lists:map(Fun,Incidences)).
corners_ff({incidences_tf,S_itf},{vertex_tf,S_vtf},{regions_f,S_rf}) -> % converter
    Fun = fun(Region) ->
        corners({incidences_tf,S_itf},{vertex_tf,S_vtf},Region) end,
    {corners_ff,lambda:tabulate(S_rf,Fun)}.
neighbors_ff({corners_ff,S_cff}) -> % converter
    Fun = fun({Region,List}) ->
        Fun = fun(Vertex) ->
            {Vertex,Region} end,
        lists:map(Fun,List) end,
    List = lambda:collect(lists:append(lists:map(Fun,S_cff))),
    Fun = fun({Vertex,List}) ->
        {Vertex,set:list(set:sort(List))} end,
    {neighbors_ff,set:list(set:sort(lists:map(Fun,List)))}.
partial_f({shell_tf,S_btf},{boundaries_f,S_bf},{regions_f,S_rf},Region) -> % converter
    % Order -> Position -> {Region,Boundary,Neighbor}
    Fun = fun(List) ->
        set:counti(List) end,
    {partial_f,set:counti(lists:map(Fun,partial_r({shell_tf,S_btf},S_bf,S_rf)))}.
partial_r({shell_tf,S_btf},Boundaries,Regions,Region) ->
    Fun = fun(Boundary) ->
        {flat,Bounds} = set:difference({flat,S_bf},set:singleton(Boundary)),
        {flat,Regs} = set:difference({flat,S_rf},set:singleton(Region)),
        Neighbor = space:neighbor(Boundary,Region),
        Fun = fun(List) ->
            [{Region,Boundary,Neighbor}|List] end,
        Lists = partial_r({shell_tf,S_btf},Bounds,Regs,Neighbor),
        lists:map(Fun,Lists) end,
    Shell = space:shell({shell_tf,S_btf},Region),
    lists:append(lists:map(Fun,Shell)).
partialb_ff({partial_t,S_ot}) -> % converter
    % Boundary -> (Order -> Position)
    Fun = fun({Order,List}) ->
        Fun = fun({Position,{Region,Boundary,Neighbor}}) ->
            {Boundary,{Order,Position}} end,
        lists:map(Fun,List) end,
    {flat,List} = set:sort(lists:append(lists:map(Fun,S_of))),
    {partialb_ff,lambda:collect(List)}.
partialr_ff({partial_t,S_of}) -> % converter
    % Region -> (Order -> Position)
    Fun = fun({Order,List}) ->
        Fun = fun({Position,{Region,Boundary,Neighbor}}) ->
            {Region,{Order,Position}} end,
        lists:map(Fun,List) end,
    {flat,List} = set:sort(lists:append(lists:map(Fun,S_of))),
    {partialr_ff,lambda:collect(List)}.
partialv_ff({partial_t,S_ot},{corners_tf,S_ctf}) -> % converter
    % Vertex -> (Order -> Position)
    Fun = fun({Order,List}) ->
        Fun = fun({Position,{Region,Boundary,Neighbor}}) ->
            Fun = fun(Corner) ->
                {Corner,{Order,Position}} end,
            {flat,One} = corners({corners_tf,S_ctf},Region),
            {flat,Other} = corners({corners_tf,S_ctf},Neighbor),
            {flat,Sect} = set:intersect({flat,One},{flat,Other}),
            lists:map(Fun,Sect) end,
        lists:append(lists:map(Fun,List)) end,
    {flat,List} = set:sort(lists:append(lists:map(Fun,S_of))),
    {partialv_ff,lambda:collect(List)}.
prior({partialv_tf,S_ovtf},{partialb_tf,S_obtf},Boundary,Vertex) -> % inducer
    [{Ord,Pos}|_] = set:get({partialv_tf,S_ovtf},Vertex),
    Pos < set:get(set:get({partialb_tf,S_obtf},Boundary),Ord).
sign({side_t,S_st},{partial_t,S_ot},{partialv_tf,S_ovtf},{side_pf,S_pf},Boundary,Vertex) -> % inducer
    [{Ord,Pos}|_] = set:get({partialv_tf,S_ovtf},Vertex),
    {Region,Bound,_} = partial({partial_t,S_ot},Ord,Pos),
    fun() when Bound == Boundary ->
        set:hole({flat,S_pf});
    () ->
        set:access({side_t,S_st},Boundary,Region) end
    ().
cosection({side_t,S1_st},{flat,Above1},{flat,Coin1},{flat,Below1}) ->
    throw([]).
cospace({side_t,S_st},{flat,Vertices}) ->
    throw([]).
cospace_r({side_t,S0_st},{side_t,S1_st},{flat,Map0},{flat,Map1},Vertex) ->
    % Map0 lists {some S0 boundaries,all S1 vertices}
    % Map1 lists {all S1 boundaries,some S0 vertices}
    {S3_st,Map3,Bound3} = cospace_s({side_t,S0_st},{side_t,S1_st},{flat,Map0},{flat,Map1},Vertex),
    % find Set3 vertices in S3 that lie on Bound3
    Fun = fun(Vert,{S_st,Map}) ->
        {S2_st,Map2,_} = cospace_r({side_t,S3_st},{side_t,S_st},{flat,Map3},{flat,Map},Vert),
        {S2_st,Map2} end,
    {S2_st,Map2} = lists:fold(Fun,Set3,{S0_st,Map0}), % apply below cospace_r with each Set2 vertex
    % return {S2_st,S3_st,Map2,Map3}
    throw([]).
cospace_s({side_t,S0_st},{side_t,S1_st},{flat,Map0},{flat,Map1},Vertex) ->
    [{Ord0,Pos0}|_] = partialv({partialv_tf,S0_vtf},Vertex),
    {flat,Bounds0} = set:domain({flat,Map0}),
    Above0 = lists:filter(Fun0,Bounds0),
    Coin0 = vertexi({vertexi_t,S_vt},Vertex),
    Below0 = lists:filter(Fun1,Bounds0),
    {flat,Above1} = set:image({flat,Map0},{flat,Above0}),
    {flat,Coin1} = set:image({flat,Map0},{flat,Coin0}),
    {flat,Below1} = set:image({flat,Map0},{flat,Below0}),
    {side_t,S2_st} = cosection({side_t,S1_st},{flat,Above1},{flat,Coin1},{flat,Below1}),
    Bound1 = set:hole({flat,S1_bf}),
    {side_t,S3_st} = space:supersection({side_t,S1_st},{side_t,S2_st},Bound1),
    % correlate Map1 to Map2 with vertices from S3
    % extend Map2 to Map3 by {Bound1,Vertex}
    % return {S3,Map3,Bound1}
    .
roundspace({side_t,S_st},{boundaries_f,S_bf},{regions_f,S_rf},{sides_f,S_pf},{linear,S_l},
    {boundaries_t,S_bt},{regions_t,S_rt},Boundary) -> % deducer
    Side = set:choose({flat,S_pf}),
    Other = set:choose(set:remove({flat,S_pf},Side)),
    {tree,Not} = set:tree(set:sort([{Side,Other},{Other,Side}])),
    {flat,Boundaries} = set:union(set:singleton(Boundary),{flat,S_bf}),
    {flat,Regions} = set:union(set:holes({flat,S_rf},S_l),{flat,S_rf}),
    Fun = fun(Boundary,Region) ->
        fun(false,false) ->
            Other;
        (false,true) ->
            Side;
        (true,false) ->
            set:get({tree,Not},set:get({side_t,S_st},Boundary,Region));
        (true,true) ->
            set:get({side_t,S_st},Boundary,Region) end
        (set:member({boundaries_t,S_bt},Boundary),set:member({regions_t,S_rt},Region)) end,
    {side_f,lambda:tabulate({flat,Boundaries},{flat,Regions},Fun)}.
signifv({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},{vertexi_t,S_vit},
    {flat,Regions},Vertex) -> % inducer
    Side = set:choose({flat,S_pf}),
    Other = set:choose(set:remove({flat,S_pf},Side)),
    {tree,Sidedness} = set:tree(set:unionk(set:singleton({false,Side}),set:singleton({true,Other}))),
    {flat,Boundaries} = vertexi({vertexi_t,S_vit},Vertex),
    Length = erlang:length(Boundaries),
    {flat,Proper} = set:sets({flat,Boundaries},Length-1),
    Fun1 = fun(Set) ->
        {flat,Subs} = set:sets({flat,Set}),
        {Set,Subs} end,
    Tuples = lists:map(Fun1,Proper),
    Fun2 = fun({Set,Subs}) ->
        % find power set of maps
        Fun = fun(Sub) ->
            {tree,Tree} = set:tree({flat,Sub}),
            Fun = fun(Bound) ->
                {Bound,set:get({tree,Sidedness},set:member({tree,Tree},Bound))} end,
            lists:map(Fun,Set) end,
        lists:map(Fun,Subs) end,
    Mapss = lists:map(Fun2,Tuples),
    Fun3 = fun(Maps) ->
        Fun2 = fun(Map) ->
            space:polycil({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},{flat,Map}) end,
        lists:map(Fun2,Maps) end,
    Polycilss = lists:map(Fun3,Mapss),
    Fun4 = fun(Polycils) ->
        Fun = fun({flat,Polycil}) ->
            set:length(set:intersect({flat,Polycil},{flat,Regions})) > 0 and
            set:length(set:difference({flat,Polycil},{flat,Regions})) > 0 end,
        lists:any(Fun,Polycils) end,
    lists:all(Fun4,Polycilss).
signifb({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{vertexi_t,S_vit},
    {flat,Regions},Boundary,Vertex) -> % inducer
    {flat,Boundaries} = vertexi({vertexi_t,S_vit},Vertex),
    {tree,Tree} = set:tree({flat,Regions}),
    {flat,Pencil} = space:pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,Boundaries}),
    Fun1 = fun(Region) ->
        set:member({tree,Tree},Region) end,
    Members = lists:filter(Fun1,Pencil),
    Fun2 = fun(Region) ->
        Neighbor = space:neighbor({dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},Boundary,Region),
        not set:member({tree,Tree},Neighbor) end,
    lists:any(Fun2,Members).
signifr({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{vertexi_t,S_vit},
    {flat,Regions},Region,Vertex) -> % inducer
    {flat,Boundaries} = vertexi({vertexi_t,S_vit},Vertex),
    Pencil = space:pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{flat,Boundaries}),
    set:member(set:tree(set:intersect({flat,Pencil},{flat,Regions})),Region).
signifv_f({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},{vertexi_t,S_vit},
    {vertices_f,S_jf},{flat,Regions}) -> % converter
    Fun = fun(Vertex) ->
        signifv({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},{vertexi_t,S_vit},
            {flat,Regions},Vertex) end,
    {signifv_f,set:list(set:sort(lists:filter(Fun,S_jf)))}.
signifb_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{vertexi_t,S_vit},
    {vertices_f,S_jf},{flat,Regions}) -> % converter
    Fun = fun(Vertex) ->
        Fun = fun(Boundary) ->
            signifb({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{vertexi_t,S_vit},
                {flat,Regions},Boundary,Vertex) end,
        {flat,Boundaries} = vertexi({vertexi_t,S_vit},Vertex),
        lists:filter(Fun,Boundaries) end,
    {signifb_ff,lambda:tabulate(S_jf,Fun)}.
signifr_ff({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{vertexi_t,S_vit},
    {vertices_f,S_jf},{flat,Regions}) -> % converter
    Fun = fun(Vertex) ->
        {flat,Boundaries} = vertexi({vertexi_t,S_vit},Vertex),
        {flat,Pencil} = space:pencil({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},
            {flat,Boundaries}),
        set:list(set:intersect({flat,Boundaries},{flat,Pencil})) end,
    {signifr_ff,lambda:tabulate(S_jf,Fun)}.
polytope({side_t,S_st},{dual_tf,_},{duali_tf,_},{boundaries_f,S_bf},{regions_f,_),{sides_f,_},
    {vertexi_t,_},{vertices_f,_},{dimension,S_n},{flat,Regions}) when S_n == 1 -> % deducer
    Fun = fun(Boundary,Region) ->
        set:access({side_t,S_st},Boundary,Region) end,
    {side_f,set:list(set:uniquefy({flat,lambda:tabulate(S_bf,Regions,Fun)}))};
polytope({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{boundaries_f,_},{regions_f,S_rf},{sides_f,S_pf},
    {vertexi_t,S_vit},{vertices_f,S_jf},{dimension,S_n},{flat,Regions}) when S_n > 0 ->
    % list of tuple of subspace dual list and subspace region list
    % apply permutes and sort before comparing
    {signifv_f,S_gvf} = signifv_f({side_t,S_st},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},
        {vertices_f,S_jf},{vertexi_t,S_vit},{flat,Regions}),
    {signifb_tf,S_gbtf} = signifb_tf({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},
        {vertexi_t,S_vit},{flat,Regions}),
    {signifr_tf,S_grtf} = signifr_tf({dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf),{sides_f,S_pf},
        {vertexi_t,S_vit},{flat,Regions}),
    Fun = fun(Vertex) ->
        % find significant boundaries of vertexi of Vertex.
        {flat,Signifb} = signifb({signifb_tf,S_gbtf},Vertex),
        % find significant regions of pencil of vertexi of Vertex.
        {flat,Signifr} = signifr({signifr_tf,S_grtf},Vertex),
        % find duals of significant regions.
        Fun3 = fun(Region) ->
            space:dual({dual_tf,S_dtf},Region) end,
        Duals = lists:map(Fun3,Signifr),
        % intersect duals with significant boundaries.
        Fun4 = fun({flat,Set}) ->
            set:intersect({flat,Set},{flat,Signifb}) end,
        Sects = lists:map(Fun4,Duals),
        % find round space, aka subspace, aka vertex space, with significant boundaries.
        {{side_t,S0_st},{dual_tf,S0_dtf},{duali_tf,S0_ditf},{boundaries_f,_},{regions_f,S0_rf),{sides_f,S0_pf},
            {incidences_tf,S_qtf},{vertexi_t,S0_vit}} = convert_sdibrpqi(
            space:subspace({dual_tf,S_dtf},{regions_f,S_rf},{sides_f,S_pf},{flat,Signifb})),
        % apply duali of round space to get round space regions.
        Fun5 = fun({flat,Set}) ->
            space:duali({duali_tf,S0_ditf},{flat,Set}) end,
        {flat,Regs} = set:uniquefy(set:sort(lists:map(Fun5,Sects))),
        % return polytope of round space.
        polytope({side_t,S0_st},{dual_tf,S0_dtf},{duali_tf,S0_ditf},{regions_f,S0_rf},{sides_f,S0_pf},
            {vertexi_t,S0_vit},{flat,Regs},{dimension,S_n-1}) end,
    lists:map(Fun,S_gvf).
embed(Polytope) ->
    % depth first superspace with different set choice each time, until vertex insides match
    throw([]).
