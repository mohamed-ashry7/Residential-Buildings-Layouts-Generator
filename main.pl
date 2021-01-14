:-use_module(library(clpfd)).








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

get_room_type(main_hallway,X,W,Y,H,Type):-
    Type = main_hallway(X,W,Y,H).

get_room_type(hallway,X,W,Y,H,Type):-
    Type = hallway(X,W,Y,H).

get_room_type(elevator,X,W,Y,H,Type):-
    Type = elevator(X,W,Y,H).

get_room_type(stairs,X,W,Y,H,Type):-
    Type = stairs(X,W,Y,H).

get_room_type(dressing_room,X,W,Y,H,Type):-
    Type = dressing_room(X,W,Y,H).

get_room_type(sun_room,X,W,Y,H,Type):-
    Type = sun_room(X,W,Y,H).


convert_types_into_rooms([],[],[]).
convert_types_into_rooms([Room|RS],[R|T],[X,Width,Y,Height|Vs]):-
    floor_dimensions(W,H),
    ((length(Room,2),Room =[Type,MinArea]);
    (length(Room,3),Room = [Type,MinArea,Width]);
    (length(Room,4),Room = [Type, MinArea,Width,Height])),
    [X,Width] ins 0..W,
    [Y,Height] ins 0..H,
    X+Width #=< W,
    Y+Height #=<H,
    RealArea #= Width*Height,
    RealArea #>=MinArea,
    get_room_type(Type,X,Width,Y,Height,R),
    convert_types_into_rooms(RS,T,Vs).






% ------------------------------------------Adjacency predicate----------------------------------------
make_adjacent(V,V).
make_adjacent(U,V):-
    U\=V,
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

get_the_sun_room([],none). % In case there is not a sun room room. 

get_the_sun_room([H|T],K):-
    H \= sun_room(_,_,_,_),
    get_the_sun_room(T,K).

get_the_sun_room([H|_],H):-
    H = sun_room(_,_,_,_).


make_a_room_on_landscape(R,O):-
    get_room_type(_,X,Wi,Y,He,R),
    open_sides(N,E,S,W),
    (
        (N=1,O=min(Y));
        (S=1,O=max(Y+He));
        (E=1,O=max(X+Wi));
        (W=1,O=min(X))
    ). 
make_the_sun_room_light(L,O):-
    get_the_sun_room(L,S),
    ((S=none, O=none);
    make_a_room_on_landscape(S,O)).

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
get_the_bedrooms_minor_bathrooms([_],[]).

get_the_bedrooms_minor_bathrooms([H,B|T],T1):-
    (H\=bedroom(_,_,_,_);B\=minor_bathroom(_,_,_,_)),
    get_the_bedrooms_minor_bathrooms([B|T],T1). 

get_the_bedrooms_minor_bathrooms([B,M|T],[B,M|T1]):-
    B = bedroom(_,_,_,_), 
    M = minor_bathroom(_,_,_,_),
    get_the_bedrooms_minor_bathrooms(T,T1).


make_the_bedrooms_minor_bathrooms_adjacent_helper([]).
make_the_bedrooms_minor_bathrooms_adjacent_helper([B,M|T]):-
    make_adjacent(B,M),
    make_the_bedrooms_minor_bathrooms_adjacent_helper(T).

make_the_bedrooms_minor_bathrooms_adjacent(L):-
    get_the_bedrooms_minor_bathrooms(L,T),
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


% ---------------------------------------------------Hallway---------------------------------------------------

has_hallway([H|_]):-
    H=[hallway|_].

has_hallway([H|T]):-
    H\=[hallway|_],
    has_hallway(T).


handle_hallway(L,NL):-
    has_hallway(L),
    NL = L. 

handle_hallway(L,NL):-
    \+has_hallway(L),
    NL = [[hallway,1]|L].



get_all_hallways([],[]). % In case there is not a dining room. 

get_all_hallways([H|T],K):-
    H \= hallway(_,_,_,_),
    get_all_hallways(T,K).

get_all_hallways([H|T],[H|T1]):-
    H = hallway(_,_,_,_),
    get_all_hallways(T,T1).


make_all_rooms_adjacent_hallway_helper([],_).
make_all_rooms_adjacent_hallway_helper([H|T],[Hallway|TH]):-
    (make_adjacent(H,Hallway);
    make_all_rooms_adjacent_hallway_helper([H],TH)),
    make_all_rooms_adjacent_hallway_helper(T,[Hallway|TH]).

make_all_rooms_adjacent_hallway(L):-
    get_all_hallways(L,HW),
    make_all_rooms_adjacent_hallway_helper(L,HW).


get_one_hallway([H|_],H):-
    H=hallway(_,_,_,_).

get_one_hallway([H|T],G):-
    H\=hallway(_,_,_,_),
    get_one_hallway(T,G).

%-------------------------------------------------Design Apartment----------------------------------------------


design_apart(L,F,Vs,Options,Hallway):-
    handle_hallway(L,NL),
    convert_types_into_rooms(NL,F,Vs),
    get_one_hallway(F,Hallway),
    make_the_kitchen_bathrooms_adjacent(F),
    make_the_sun_room_light(F,O),
    ((Options=[],O=none);Options=[]),
    make_the_dressing_rooms_bedrooms_adjacent(F),
    make_the_bedrooms_minor_bathrooms_adjacent(F),
    make_the_kitchen_dining_room_adjacent(F),
    make_all_rooms_adjacent_hallway(F).
    

design_floor_helper([],[],[],[],[]).
design_floor_helper([H|T],[F|T1],[Vs|T2],[Options|T3],[Hallway|T4]):-
    design_apart(H,F,Vs,Options,Hallway),
    design_floor_helper(T,T1,T2,T3,T4).




add_elevator_stairs(ElevatorStairs,Coordinates):-
    floor_dimensions(W,H),
    ElevatorStairs =[elevator(XE,1,YE,1),stairs(XS,1,YS,1)],
    Coordinates= [XE,YE,XS,YS],
    [XE,XS] ins 0..W,
    [YE,YS] ins 0..H,
    XE + 1 #=<W,
    XS +1 #=<W,
    YE +1 #=<H,
    YS +1 #=<H.


add_main_hallway(MainHallWay,MainHallWayVars):-
    floor_dimensions(Width,Height),
    MainHallWay = main_hallway(MX,MW,MY,MH),
    MainHallWayVars = [MX,MW,MY,MH],
    [MX,MW] ins 0..Width,
    [MY,MH] ins 0..Height,
    MX +MW #=<Width,
    MY +MH #=<Height.


calc_area(H,A):-
    get_room_type(_,_,Width,_,Height,H),
    A #= Width*Height.

get_the_whole_area([],0).
get_the_whole_area([H|T],A):-
    calc_area(H,G),
    A #= G+A1,
    get_the_whole_area(T,A1).

option_divine_propotion([]).
option_divine_propotion([R|T]):-
    floor_dimensions(Width,_),
    get_room_type(_,_,W,_,H,R),
    (var(W),
    var(H),
    B in Width,
    H+B#=W,
    W//H #= H//B;true),
    option_divine_propotion(T). 


has_sunroom([R|_]):-
    R =sun_room(_,_,_,_).
has_sunroom([R|T]):-
    R\=sun_room(_,_,_,_),
    has_sunroom(T).

make_a_apart_on_landscape([],_).
make_a_apart_on_landscape([R|T],O):-
    make_a_room_on_landscape(R,O);make_a_apart_on_landscape(T,O).


option_landscape([],[]).

option_landscape([A|T],[O|T1]):-
    (has_sunroom(A);\+has_sunroom(A),make_a_apart_on_landscape(A,O)),
    option_landscape(T,T1).


option_equal_distance_to_elevator_helper([],_,[]).

option_equal_distance_to_elevator_helper([A|T],E,[D|T1]):-
    get_one_hallway(A,Hall),
    get_room_type(_,X1,_,Y1,_,E),
    get_room_type(_,X2,W,Y2,H,Hall),
    D #= (X1-(X2+W//2))^2 + (Y1-(Y2+H//2))^2,
    option_equal_distance_to_elevator_helper(T,E,T1).
    
option_equal_distance_to_elevator(As,E):-
    option_equal_distance_to_elevator_helper(As,E,Ds),
    all_the_same(Ds).

all_the_same([_]).
all_the_same([A,B|T]):-
    A #=B,
    all_the_same([B|T]).  


option_symmetry([]).

handle_optional_global_constraints(F,Options):-
    optional_global(LandscapeLook,DistanceToElevator,Symmetry,Divine), 
    [[E,_],_|Apartments] = F,
    flatten(F, G),
    (LandscapeLook =1, option_landscape(Apartments,Options);LandscapeLook=0,Options=[]),
    (DistanceToElevator =1, option_equal_distance_to_elevator(Apartments,E);DistanceToElevator=0),
    (Symmetry =1, option_symmetry([]);Symmetry=0),
    (Divine =1, option_divine_propotion(G);Divine=0).
% ----------------------------------------------Soft Constraints--------------------------------------------------

calc_exposed_fn([],0).
calc_exposed_fn([R|A],C):-
    get_room_type(_,X,W,Y,H,R),
    open_sides(No,Ea,So,We),
    floor_dimensions(Width,Height),
    ((
    (X#<5,We=1;
    Y#<5,No=1;
    X+W#<Width,X+W#>Width-5,Ea=1;
    Y+H#<Height,Y+H#>Height-5,So=1),
    C1=0);C1=1),
    C #= C1+C2,
    calc_exposed_fn(A,C2). 

get_all_bedrooms([],[]).
get_all_bedrooms([R|A],[R|T1]):-
    get_room_type(bedroom,_,_,_,_,R),get_all_bedrooms(A,T1).

get_all_bedrooms([R|A],T1):-
    get_room_type(Type,_,_,_,_,R),Type\=bedroom,get_all_bedrooms(A,T1). 


calc_abs_distance(F,G,C):-
    get_room_type(_,X,_,Y,_,F),
    get_room_type(_,X1,_,Y1,_,G),
    C #= abs(X-X1)+abs(Y-Y1).

calc_bedroom_closer([_],0).
calc_bedroom_closer([F,G|A],C):-
    calc_abs_distance(F,G,C2),
    C #= C2+C1,
    calc_bedroom_closer([G|A],C1).

get_all_bathrooms([],[]).
get_all_bathrooms([R|A],[R|T1]):-
    get_room_type(master_bathroom,_,_,_,_,R),get_all_bathrooms(A,T1).

get_all_bathrooms([R|A],T1):-
    get_room_type(Type,_,_,_,_,R),
    Type\=master_bathroom,
    get_all_bathrooms(A,T1). 

calc_bathroom_fn_helper([R|A],[B|Bs],C):-
    calc_abs_distance(R,B,C1),
    C #=C1+C2,
    calc_bathroom_fn_helper(A,Bs,C2).

calc_bathroom_fn(A,C):-
    get_all_bathrooms(A,Bs),
    calc_bathroom_fn_helper(A,Bs,C).

calc_distance_fn([_],0).

calc_distance_fn([R,R1|T],D):-
    
    calc_abs_distance(R,R1,C),(C#>5,D=1;C#=<5,D=0);calc_distance_fn([R1|T],D). 




implement_soft_constraints([],[],0).

implement_soft_constraints([A|T1],[S|C],V):-
    [ExposedDayLight,Distance,BedroomsCloser,MainBathroom] = S, 
    (ExposedDayLight=1,calc_exposed_fn(A,EV);ExposedDayLight=0,EV=0),
    (Distance=1,calc_distance_fn(A,DV);Distance=0,DV=0),
    (BedroomsCloser=1,get_all_bedrooms(A,Bs),calc_bedroom_closer(Bs,BV);BedroomsCloser=0,BV=0),
    (MainBathroom=1,calc_bathroom_fn(A,MBV);MainBathroom=0,MBV=0),
    V #= EV+DV+BV+MBV+V1,
    implement_soft_constraints(T1,C,V1).

design_floor(As,SoftConstraints,F):-
    length(As,NumberApartments),
    length(SoftConstraints,NumberApartments) , %To make sure every apartment has its soft constraints.
    implement_soft_constraints(FF,SoftConstraints,CostVars),
    add_main_hallway(MainHallWay,MainHallWayVars),
    add_elevator_stairs(ES,Coor),
    make_adjacent_lists(MainHallWay,ES),
    design_floor_helper(As,FF,VF,OF,Hallways),
    make_adjacent_lists(MainHallWay,Hallways),
    Vs = [Coor,MainHallWayVars|VF],
    flatten(Vs, Variables),
    F = [ES,MainHallWay|FF],
    flatten(F, Functors),
    get_the_whole_area(Functors,Area),
    handle_optional_global_constraints([ES,MainHallWay|FF],GlobalOptions),
    OFA = [max(Area),min(CostVars),GlobalOptions|OF],
    flatten(OFA, Options),
    disjoint2(Functors),
    labeling(Options,Variables).


floor_dimensions(100,200).
%            Width,Height
open_sides(1,1,0,1).
%          N,E,S,W
optional_global(1,1,1,1).
%  [LandscapeLook,DistanceToElevator,Symmetry,Divine]


test0(F):-
    As = [A1,A2],
    A1 = [R1,R2,R3],
    R1=[bedroom,50],
    R2=[minor_bathroom,20] ,
    R3=[kitchen,10],
    A2 = [R4,R5,R6],
    R4=[bedroom,10],
    R5=[minor_bathroom,10] ,
    R6=[kitchen,25] ,
    Soft = [[1,1,1,1],[0,0,0,0]],
    design_floor(As,Soft,F).


test1(Fs):-
    % F = [bedroom(0, 1, 0, 50),hallway(1,1,1,1), minor_bathroom(1, 1, 0, 20), kitchen(0, 1, 50, 123)],
    % has_hallway(F).
    Vs = [X,W,Y,H,X1,W1,Y1,H1],
    Vs ins 1..100,
    Fs = [F1,F2],
    F1 = hallway(X,W,Y,H),
    F2 = kitchen(X1,W1,Y1,H1),
    % make_adjacent_lists(F1,[F1,F2]),
    make_all_rooms_adjacent_hallway(Fs),
    disjoint2(Fs),
    labeling([],Vs). 
    

test2(F):-
    Divine = 1 ,
    A1 = [R1,R2,R3],
    R2=[bedroom,50],
    R1=[minor_bathroom,20] ,
    R3=[kitchen,10],
    design_apart(A1,F,Vs,O,_),
    (Divine =1,option_divine_propotion(F) ;Divine = 0 ),
    flatten(O, Options),
    disjoint2(F),
    labeling(Options,Vs).



test5(F):-
    F #= (X1-X2)^2,
    [X1,X2] ins 1..9,
    all_distinct([X1,X2]).
    
% test3(F):-

