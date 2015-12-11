-module(lambda).
-export([zip/1,zip/2,zip/3,zip/4,zip/5,zip/6]).
-export([cross/1,cross/2,cross/3,cross/4,cross/5,cross/6]).
-export([deep/2,tabulate/2,tabulate/3,tabulate/4]).
-export([collect/1]).

zip(List0) ->
    Fun = fun(Elem0) ->
        {Elem0} end,
    lists:map(Fun,List0).
zip(List0,List1) ->
    Fun = fun({Elem0,Elem1}) ->
        {Elem0,Elem1} end,
    lists:map(Fun,lists:zip(List0,List1)).
zip(List0,List1,List2) ->
    Fun = fun({Elem0,{Elem1,Elem2}}) ->
        {Elem0,Elem1,Elem2} end,
    lists:map(Fun,lists:zip(List0,lists:zip(List1,List2))).
zip(List0,List1,List2,List3) ->
    Fun = fun({Elem0,{Elem1,{Elem2,Elem3}}}) ->
        {Elem0,Elem1,Elem2,Elem3} end,
    lists:map(Fun,lists:zip(List0,lists:zip(List1,lists:zip(List2,List3)))).
zip(List0,List1,List2,List3,List4) ->
    Fun = fun({Elem0,{Elem1,{Elem2,{Elem3,Elem4}}}}) ->
        {Elem0,Elem1,Elem2,Elem3,Elem4} end,
    lists:map(Fun,lists:zip(List0,lists:zip(List1,lists:zip(List2,lists:zip(List3,List4))))).
zip(List0,List1,List2,List3,List4,List5) ->
    Fun = fun({Elem0,{Elem1,{Elem2,{Elem3,{Elem4,Elem5}}}}}) ->
        {Elem0,Elem1,Elem2,Elem3,Elem4,Elem5} end,
    lists:map(Fun,lists:zip(List0,lists:zip(List1,lists:zip(List2,lists:zip(List3,lists:zip(List4,List5)))))).
cross(List0) ->
    Fun = fun(Elem0) ->
        {Elem0} end,
    lists:map(Fun,List0).
cross(List0,List1) ->
    Fun = fun(Elem0) ->
        Fun = fun(Elem1) ->
            {Elem0,Elem1} end,
        lists:map(Fun,List1) end,
    lists:append(lists:map(Fun,List0)).
cross(List0,List1,List2) ->
    Fun = fun(Elem0) ->
        Fun = fun({Elem1,Elem2}) ->
            {Elem0,Elem1,Elem2} end,
        lists:map(Fun,cross(List1,List2)) end,
    lists:append(lists:map(Fun,List0)).
cross(List0,List1,List2,List3) ->
    Fun = fun(Elem0) ->
        Fun = fun({Elem1,Elem2,Elem3}) ->
            {Elem0,Elem1,Elem2,Elem3} end,
        lists:map(Fun,cross(List1,List2,List3)) end,
    lists:append(lists:map(Fun,List0)).
cross(List0,List1,List2,List3,List4) ->
    Fun = fun(Elem0) ->
        Fun = fun({Elem1,Elem2,Elem3,Elem4}) ->
            {Elem0,Elem1,Elem2,Elem3,Elem4} end,
        lists:map(Fun,cross(List1,List2,List3,List4)) end,
    lists:append(lists:map(Fun,List0)).
cross(List0,List1,List2,List3,List4,List5) ->
    Fun = fun(Elem0) ->
        Fun = fun({Elem1,Elem2,Elem3,Elem4,Elem5}) ->
            {Elem0,Elem1,Elem2,Elem3,Elem4,Elem5} end,
        lists:map(Fun,cross(List1,List2,List3,List4,List5)) end,
    lists:append(lists:map(Fun,List0)).
deep([H|T],Fun) ->
    Fun(map([H|T],Fun));
deep(Elem,Fun) ->
    Fun(Elem).
tabulate(List,Giv) ->
    Fun = fun(Elem) ->
        {Elem,Giv(Elem)} end,
    lists:map(Fun,List).
tabulate(List0,List1,Giv) ->
    Fun = fun(Elem0) ->
        Fun = fun(Elem1) ->
            Giv(Elem0,Elem1) end,
        {Elem0,tabulate(List1,Fun)} end,
    lists:map(Fun,List0).
tabulate(List0,List1,List2,Giv) ->
    Fun = fun(Elem0) ->
        Fun = fun(Elem1,Elem2) ->
            Giv(Elem0,Elem1,Elem2) end,
        {Elem0,tabulate(List1,List2,Fun)} end,
    lists:map(Fun,List0).
collect(List) ->
    % map unique keys to lists of values
    Fun = fun({Key,Val},[{Last,List}|Tail]) when Key == Last ->
        [{Last,[Val|List]}|Tail];
    ({Key,Val},[{Last,List}|Tail]) when Key /= Last ->
        [{Key,[Val]}|Tail];
    ({Key,Val},[]) ->
        [{Key,[Val]}] end,
    lists:fold(Fun,List,[]).
index(List) ->
    % map position list keys to leaves
    index_r(List,[0]).
index_r([[H|D]|T],[Index|Indices]) ->
    index_r([H|D],[0,Index|Indices])++index_r(T,[Index+1|Indices]);
index_r([H|T],[Index|Indices]) ->
    [H,{lists:reverse([Index|Indices])}|index_r(T,[Index+1|Indices])];
index_r([],_) ->
    [].
