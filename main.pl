:-use_module(library(clpfd)).


floor_width(100).
floor_height(200).

open_sides(1,1,0,1).
%          N,E,S,W





get_room_type(bedroom,X,W,Y,H,Type):-
    Type = bedroom(X,W,Y,H).

get_room_type(kitchen,X,W,Y,H,Type):-
    Type = kitchen(X,W,Y,H).

get_room_type(dining_room,X,W,Y,H,Type):-
    Type = dining_room(X,W,Y,H).

get_room_type(master_bathroom,X,W,Y,H,Type):-
    Type = master_bathroom(X,W,Y,H).

get_room_type(minor_bathroom,X,W,Y,H,Type):-
    Type = minor_bathroom(X,W,Y,H).

get_room_type(living_room,X,W,Y,H,Type):-
    Type = living_room(X,W,Y,H).

get_room_type(hallway,X,W,Y,H,Type):-
    Type = hallway(X,W,Y,H).

get_room_type(dressing_room,X,W,Y,H,Type):-
    Type = dressing_room(X,W,Y,H).

get_room_type(sun_room,X,W,Y,H,Type):-
    Type = sun_room(X,W,Y,H).


convert_types_into_rooms([],[],[]).
convert_types_into_rooms([Room|RS],W,H,[R|T],[X,Width,Y,Height|Vs]):-
    ((length(Room,2),Room =[Type,MinArea]);
    (length(Room,3),Room = [Type,MinArea,Width]);
    (length(Room,4),Room = [Type, MinArea,Width,Height])),
    X in 0..W,
    Y in 0..H,
    Width in 1..W,
    Height in 1..H,
    X+Width #=< W,
    Y+Height #=<H,
    RealArea #= Width*Height,
    RealArea #>=MinArea,
    get_room_type(Type,X,W,Y,H,R),
    convert_types_into_rooms(RS,W,H,T,Vs).






% ------------------------------------------Adjacency predicate----------------------------------------
make_adjacent(U,V):-
    get_room_type(_,X1,W1,Y1,H1,U),
    get_room_type(_,X2,W2,Y2,H2,V),
    (
    X2 #= X1+W1;
    X1 #= X2+W2;
    Y2 #= H1+Y1;
    Y1 #= H2+Y2
    ).
make_adjacent_lists(_,[]).
make_adjacent_lists(U,[V|Vs]):-
    make_adjacent(U,V),
    make_adjacent_lists(U,Vs).


% -----------------------------------------The kitchens and Bathrooms-----------------------------------------------------------
get_the_kitchen([H|T],K):-
    H \= kitchen(_,_,_,_),
    get_the_kitchen(T,K).

get_the_kitchen([H|_],H):-
    H = kitchen(_,_,_,_).

get_the_bathrooms([],[]).
get_the_bathrooms([H|T],T1):-
    H \= master_bathroom(_,_,_,_),
    H \= minor_bathroom(_,_,_,_),
    get_the_bathrooms(T,T1).

get_the_bathrooms([H|T],[H|T1]):-
    (H = master_bathroom(_,_,_,_);
    H = minor_bathroom(_,_,_,_)),
    get_the_bathrooms(T,T1).


make_the_kitchen_bathrooms_adjacent(L):-
    get_the_kitchen(L,K),
    get_the_bathrooms(L,Bs),
    make_adjacent_lists(K,Bs).


%-------------------------------------------------------Sun room --------------------------------------------------------------------------

get_the_sun_room([],none). % In case there is not a dining room. 

get_the_sun_room([H|T],K):-
    H \= sun_room(_,_,_,_),
    get_the_sun_room(T,K).

get_the_sun_room([H|_],H):-
    H = sun_room(_,_,_,_).


make_the_sun_room_light(L,O):-
    get_the_sun_room(L,S),
    S = sun_room(X,_,Y,_),
    open_sides(N,E,S,W),
    (   S=none;
        (N=1,O=min(Y));
        (S=1,O=max(Y));
        (E=1,O=max(X));
        (W=1,O=min(X))
    ).


% ------------------------------------------------------Dressing Rooms and Bedrooms----------------------------------------------------------
get_the_dressing_rooms_bedrooms([],[]).

get_the_dressing_rooms_bedrooms([H|T],T1):-
    H\=dressing_room(_,_,_,_),
    get_the_dressing_rooms_bedrooms(T,T1). 

get_the_dressing_rooms_bedrooms([D,B|T],[D,B|T1]):-
    D = dressing_room(_,_,_,_), 
    B = bedroom(_,_,_,_),
    get_the_dressing_rooms_bedrooms(T,T1).


make_the_dressing_rooms_bedrooms_adjacent_helper([]).
make_the_dressing_rooms_bedrooms_adjacent_helper([D,B|T]):-
    make_adjacent(D,B),
    make_the_dressing_rooms_bedrooms_adjacent_helper(T).

make_the_dressing_rooms_bedrooms_adjacent(L):-
    get_the_dressing_rooms_bedrooms(L,T),
    make_the_dressing_rooms_bedrooms_adjacent_helper(T).





%------------------------------------------------Minor Bathrooms and Bedrooms-------------------------------------

get_the_bedrooms_minor_bathrooms([],[]).

get_the_bedrooms_minor_bathrooms([H|T],T1):-
    H\=bedroom(_,_,_,_),
    get_the_bedrooms_minor_bathrooms(T,T1). 

get_the_bedrooms_minor_bathrooms([B,M|T],[B,M|T1]):-
B = bedroom(_,_,_,_), 
M = minor_bathroom(_,_,_,_),
get_the_bedrooms_minor_bathrooms(T,T1).


make_the_bedrooms_minor_bathrooms_adjacent_helper([]).
make_the_bedrooms_minor_bathrooms_adjacent_helper([B,M|T]):-
    make_adjacent(B,M),
    make_the_bedrooms_minor_bathrooms_adjacent_helper(T).

make_the_bedrooms_minor_bathrooms_adjacent(L):-
    get_the_dressing_rooms_bedrooms(L,T),
    make_the_bedrooms_minor_bathrooms_adjacent_helper(T).
%-------------------------------------------------Dining Room and Kitchen------------------------------------------



get_the_dining_room([],none). % In case there is not a dining room. 

get_the_dining_room([H|T],K):-
    H \= dining_room(_,_,_,_),
    get_the_dining_room(T,K).

get_the_dining_room([H|_],H):-
    H = dining_room(_,_,_,_).    


make_the_kitchen_dining_room_adjacent(L):-
    get_the_kitchen(L,K),
    get_the_dining_room(L,D),
    (
    (D \=none , make_adjacent(K,D));
        D = none
    ).




%--------------------------------------------------Design Apartment----------------------------------------------


design_apart(L,W,H,F):-
    convert_types_into_rooms(L,W,H,F,Vs),
    make_the_kitchen_bathrooms_adjacent(L),
    make_the_sun_room_light(L,O),
    make_the_dressing_rooms_bedrooms_adjacent(L),
    make_the_bedrooms_minor_bathrooms_adjacent_helper(L),
    make_the_kitchen_dining_room_adjacent(L),
    disjoint2(F),
    labeling([O],Vs).
    


test(F):-
    Rs = [R1,R2,R3],
    R1=[bedroom,50],
    R2=[minor_bathroom,20] ,
    R3=[kitchen,10],
    design_apart(Rs,100,200,F).
