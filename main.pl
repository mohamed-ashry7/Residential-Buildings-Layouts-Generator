:-use_module(library(clpfd)).



room_type()
convert_types_into_rooms([],[],[]).
convert_types_into_rooms([Room|RS],[R|T],[X,Width,Y,Height|Vs]):-
    ((length(Room,2),Room =[Type,Area]);
    (length(Room,3),Room = [Type,Area,Width]);
    (length(Room,4),Room = [Type, Area,Width,Height])),
    [X,Width,Y,Height] ins 1..1000, % TODO 
    Width*Height #>=Area,
    R = room(X,Width,Y,Height),
    convert_types_into_rooms(RS,T,Vs).



find(L,F):-
    convert_types_into_rooms(L,F,Vs),
    disjoint2(F),
    
    labeling([],Vs).
    
    
% solve(Area,OpenSides,Apartments,ApartmentsNumber):-

%     Area = [Width,Height],


% solve(Area,SidesN,Sides,TypesN,Types,RperT,AperT):-  
%     % Area [width,height],
%     % SidesN=>    number of open sides
%     % Sides=> The sides that are open 
%     % typesN=>    Number of needed types
%     % Types=> list of lists of rooms specifying the different rooms per type
%     % RperT=> Number of rooms per type
%     % AperT=> number of appartments per type.
%     Area=[W,H],
%     %Area ins 1..sub,
%     length(Sides,SidesN),
%     Sides ins 1..4,% 1=>North,2=>south,3=>east,4=>west.
%     length(Types,TypesN),
%     %Type of rooms: (Bedroom, Kitchen, Dining Room, Master bathroom, Minor bathroom, Living Room, Hallway, Dressing room and Sun room);
%     %room given a value  1       2       3               4       5       6                  7        8           9               10
%     length(RperT,TypesN),
%     length(AperT,TypesN),
%     rooms_per_type(Types,RperT).  % check that every type has its corresponding number of rooms. rooms_per_type([[A],[B,C],[D,E,F]],[1,2,3]) evaluates to true.

%     %RperT ins 1..sub,
%     %AperT ins 1..sub.

    
% rooms_per_type([],[]).
% rooms_per_type([H|T],[H1|T1]):-
%     length(H,H1),
%     H ins 1..10,                        %the different types of rooms 
%     rooms_per_type(T,T1).

