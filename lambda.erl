-module(lambda).
-export([convert/3,convert/4,convert/5,convert/6,convert/7,select/2,table/2]).
-export([zip/1,zip/2,zip/3,zip/4,zip/5,zip/6]).
-export([cross/1,cross/2,cross/3,cross/4,cross/5,cross/6]).
-export([deep/2,tabulate/2,tabulate/3,tabulate/4]).
convert([H|T],{Tag,Tab},Fun1) ->
    lists:reverse(convert([H|T],{Tag,Tab},Fun1,fun convert_r/3,fun convert_r/4,fun convert_r/5,fun convert_r/6)).
convert([H|T],{Tag,Tab},Fun1,Fun2) ->
    lists:reverse(convert([H|T],{Tag,Tab},Fun1,Fun2,fun convert_r/4,fun convert_r/5,fun convert_r/6)).
convert([H|T],{Tag,Tab},Fun1,Fun2,Fun3) ->
    lists:reverse(convert([H|T],{Tag,Tab},Fun1,Fun2,Fun3,fun convert_r/5,fun convert_r/6)).
convert([H|T],{Tag,Tab},Fun1,Fun2,Fun3,Fun4) ->
    lists:reverse(convert([H|T],{Tag,Tab},Fun1,Fun2,Fun3,Fun4,fun convert_r/6)).
convert([H|T],{Tag,Tab},Fun1,Fun2,Fun3,Fun4,Fun5) ->
    lists:reverse(convert([H|T],{Tag,Tab},Fun1,Fun2,Fun3,Fun4,Fun5)).
convert_r(_,_,_) ->
    throw([]).
convert_r(_,_,_,_) ->
    throw([]).
convert_r(_,_,_,_,_) ->
    throw([]).
convert_r(_,_,_,_,_,_) ->
    throw([]).
convert_r([H|T],{Tag0,Tab0},Acc,false,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun1(H,{Tag0,Tab0}),
        convert_r(T,{Tag0,Tab0},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([H|T],{Tag0,Tab0},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun1(H,{Tag0,Tab0}),
        convert_r(T,{Tag0,Tab0},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([],_,Acc,_,_,_,_,_,_) ->
    Acc.
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},Acc,false,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun2(H,{Tag0,Tag0},{Tag1,Tab1}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun2(H,{Tag0,Tag0},{Tag1,Tab1}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([],_,_,Acc,_,_,_,_,_,_) ->
    Acc.
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},Acc,false,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([],_,_,_,Acc,_,_,_,_,_,_) ->
    Acc.
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},Acc,false,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun4(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([],_,_,_,_,Acc,_,_,_,_,_,_) ->
    Acc.
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},{Tag4,Tab4},Acc,false,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        convert_r(T,{Tag,Tab},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) catch _ ->
        Conv = Fun5(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},{Tag4,Tab4}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},{Tag4,Tab4},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([H|T],{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},{Tag4,Tab4},Acc,true,Fun1,Fun2,Fun3,Fun4,Fun5) ->
    try select(H,Acc) of {Tag,Tab} ->
        throw([]) catch _ ->
        Conv = Fun3(H,{Tag0,Tag0},{Tag1,Tab1},{Tag2,Tab2}),
        convert_r(T,{Tag0,Tab0},{Tag1,Tab1},{Tag2,Tab2},{Tag3,Tab3},{Tag4,Tab4},[Conv|Acc],false,Fun1,Fun2,Fun3,Fun4,Fun5) end;
convert_r([],_,_,_,_,Acc,_,_,_,_,_,_) ->
    Acc.
select(Tag,[{Tag,Tab}|T]) ->
    {Tag,Tab};
select(To,[{Tag,Tab}|T]) ->
    select(To,T).
table(To,From) ->
    {Tag,Tab} = select(To,From),
    Tab.
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
    Fun = fun({Key,Val},[{Last,List}|Tail]) when Key == Last ->
        [{Last,[Val|List]}|Tail];
    ({Key,Val},[{Last,List}|Tail]) when Key /= Last ->
        [{Key,[Val]}|Tail];
    ({Key,Val},[]) ->
        [{Key,[Val]}] end,
    lists:fold(Fun,List,[]).
