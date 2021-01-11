:-use_module(library(clpfd)).



% helper([H|T],[H1,T1]):-
%     H1#=H+1
% test(X,Y):-
%     helper(X,Y).


ss([],0).
ss([H|T],Sum):-
    Sum#=NSum+H,
    ss(T,NSum). 



get_room_type(room,X,W,Y,H,Type):-
    Type = room(X,W,Y,H).
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
len(100).


aa(X):-
    X=1;X=2.
tt(X,max(X)).
s(L):-
    len(Len),
    L=[R1,R2,R3],
    Vars = [X1,X2,Y1,Y2,W1,W2,H1,H2,X3,Y3,H3,W3], 
    Vars ins 0..Len,
    R1 = room(X1,W1,Y1,H1),
    R2 = room(X2,W2,Y2,H2),
    R3 = room(X3,W3,Y3,H3), 
    make_adjacent_lists(R1,[R2,R3]),
    Area1 #= W1*H1,
    Area2 #= W2*H2,
    W1+X1 #=<Len,
    W2+X2 #=<Len,
    H1+Y1 #=<Len,
    H2+Y2 #=<Len,
    Area1 #>=20,
    Area2 #>=50,
    % LeftArea #=Len * Len -Area1+Area2,
    disjoint2(L),
    tt(X1,A),
    labeling([A], Vars).    


% try(MinArea,X,Y):-
%     Area#=X*Y,
%     Area #>= MinArea,
%     [X,Y] ins 1..100,
%     labeling([max(Area)],[X,Y]).
    