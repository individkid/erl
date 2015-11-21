-module(polytope).
-export([inside/2,outside/2,inside/4,outside/4]).
-export([incidences/2,incidences/6,vertex/2,vertexi/2]).
-export([corners/2,corners/3,neighbors/2]).
-export([partial/3,partialb/2,partialr/2,partialv/2]).
-export([prior/4,sign/6,cosection/4,cospace/2]).
%%
%% inside_f outside_f incidences_ff vertex_f vertexi_f
%% vertices_f corners_ff neighbors_ff
%% partial_f partialb_ff partialr_ff partialv_ff
%%
%% S_if S_of S_iff S_vf S_vif
%% S_jf S_cff S_nff
%% S_of S_obff S_orff S_ovff
%%
convert([H|T],{Tag,Tab}) ->
    lambda:convert([H|T],{Tag,Tab},[{Tag,Tab}],false,fun convert/2,fun convert/3,fun convert/4,fun convert/5,fun convert/6);
convert(inside_t,{inside_f,Tab_f}) ->
    {inside_t,set:list(set:tree({flat,Tab_f}))};
convert(outside_t,{outside_f,Tab_f}) ->
    {outside_t,set:list(set:tree({flat,Tab_f}))};
convert(incidences_tf,{incidences_ff,Tab_ff}) ->
    {incidences_tf,set:list(set:semit({flat,Tab_ff}))};
convert(_,{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_}) ->
    throw([]).
convert(inside_f,{half_tf,S_htf},{shell_tf,S_btf},{regions_f,S_rf},{dimension,Dim}) ->
    inside_f({half_tf,S_htf},{shell_tf,S_btf},{regions_f,S_rf},{dimension,Dim});
convert(_,{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
convert(_,{_,_},{_,_},{_,_},{_,_},{_,_}) ->
    throw([]).
convert(incidences_ff,{shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{dimension,Dim}) ->
    incidences_ff({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{dimension,Dim});
convert(_,{_,_},{_,_},{_,_},{_,_},{_,_},{{_,_}) ->
    throw([]).
inside({inside_t,Set},Region) -> % accessor
    set:member({tree,Set},Region).
outside({outside_t,Set},Region) -> % accessor
    set:member({tree,Set},Region).
inside({half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region) -> % inducer
    {flat,Shell} = space:shell({shell_tf,S_btf},Region),
    {flat,Sets} = set:sets({flat,Shell},Dim),
    Fun0 = fun({flat,Set}) ->
        Fun = fun(Region) ->
            space:half({half_tf,S_htf},Region) end,
        set:intersectf(set:sort(lists:map(Fun,Set))) end,
    Fun1 = fun(List) ->
        erlang:length(List) == 0 end,
    erlang:length(lists:filter(Fun1,lists:map(Fun0,Sets))) > 0.
outside({half_tf,S_htf},{shell_tf,S_btf},{dimension,Dim},Region) -> % inducer
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
incidences({incidences_tf,S_ctf},Region) -> % accessor
    {flat,set:member({tree,S_ctf},Region)}.
incidences_ff({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{regions_f,S_rf},{sides_f,S_pf},{dimension,Dim}) ->
    % converter
    Fun = fun(Region) ->
        set:list(incidences({shell_tf,S_btf},{dual_tf,S_dtf},{duali_tf,S_ditf},{sides_f,S_pf},{dimension,Dim},Region)) end,
    {incidences_ff,lambda:tabulate(S_rf)}.
vertex({vertex_t,S_vt},{flat,Incidence}) -> % accessor
    set:get({tree,S_vt},Incidence).
vertex_f({incidences_tf,S_itf},{regions_f,S_rf}) -> % converter
    Fun = fun(Region) ->
        set:list(incidences({incidences_tf,S_vt},Region)) end,
    {vertex_f,set:count(lists:append(lists:map(Fun,S_rf)))}.
vertexi({vertexi_t,S_vit},Vertex) ->
    {flat,set:get({tree,S_vit},Vertex)}.
vertexi_f({vertex_f,S_vf}) -> % converter
    {vertexi_f,set:flat(set:inverse(S_vf))}.
vertices_f({vertex_f,S_vf}) -> % converter
    {vertices_f,set:list(set:range({flat,S_vf}))}.
corners({corners_tf,S_ctf},Region) -> % accessor
    {flat,set:get({tree,S_ctf},Region)}.
corners({incidences_tf,S_itf},{vertex_t,S_vt},Region) -> % inducer
    Fun = fun(List) ->
        set:get({tree,S_vt},List) end,
    Incidences = incidences({incidences_tf,S_itf},Region),
    set:sort(lists:map(Fun,Incidences)).
corners_ff({incidences_tf,S_itf},{vertex_t,S_vt},{regions_f,S_rf}) -> % converter
    Fun = fun(Region) ->
        corners({incidences_tf,S_itf},{vertex_t,S_vt},Region) end,
    {corners_ff,lambda:tabulate(S_rf,Fun)}.
neighbors({neighbors_tf,S_ntf},Vertex) -> % accessor
    set:get({tree,S_ntf},Vertex).
neighbors_ff({corners_ff,S_cff}) -> % constructor
    Fun = fun({Region,List}) ->
        Fun = fun(Vertex) ->
            {Vertex,Region} end,
        lists:map(Fun,List) end,
    List = lambda:collect(lists:append(lists:map(Fun,S_cff))),
    Fun = fun({Vertex,List}) ->
        {Vertex,set:list(set:sort(List))} end,
    {neighbors_ff,set:list(set:sort(lists:map(Fun,List)))}.
partial({partial_t,S_ot},Order,Position) -> % accessor
    % Order -> Position -> {Region,Boundary,Neighbor}
    set:get(set:get(S_ot,Order),Position).
partial_f({shell_tf,S_btf},{boundaries_f,S_bf},{regions_f,S_rf},Region) -> % converter
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
partialb({partialb_tf,S_otf},Boundary) -> % accessor
    % Boundary -> (Order -> Position)
    {flat,set:get({tree,S_otf},Boundary)}.
partialb_ff({partial_t,S_ot}) -> % constructor
    Fun = fun({Order,List}) ->
        Fun = fun({Position,{Region,Boundary,Neighbor}}) ->
            {Boundary,{Order,Position}} end,
        lists:map(Fun,List) end,
    {flat,List} = set:sort(lists:append(lists:map(Fun,S_of))),
    {partialb_ff,lambda:collect(List)}.
partialr({partialr_tf,S_otf},Region) -> % accessor
    % Region -> (Order -> Position)
    {flat,set:get({tree,S_otf},Region)}.
partialr_ff({partial_t,S_of}) -> % constructor
    Fun = fun({Order,List}) ->
        Fun = fun({Position,{Region,Boundary,Neighbor}}) ->
            {Region,{Order,Position}} end,
        lists:map(Fun,List) end,
    {flat,List} = set:sort(lists:append(lists:map(Fun,S_of))),
    {partialr_ff,lambda:collect(List)}.
partialv({partialv_tf,S_otf},Vertex) -> % accessor
    % Vertex -> (Order -> Position)
    {flat,set:get({tree,S_otf},Vertex)}.
partialv_ff({partial_t,S_ot},{corners_tf,S_ctf}) -> % constructor
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
        space:side({side_t,S_st},Boundary,Region) end
    ().
cospace({side_t,S_st},{flat,Vertices}) ->
    .
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
    .
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
