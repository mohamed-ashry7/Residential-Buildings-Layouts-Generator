:- use_module(library(clpfd)).



% helper([H|T],[H1,T1]):-
%     H1#=H+1
% test(X,Y):-
%     helper(X,Y).
apart_handler(L):-
    L=[R1,R2],
    Vars = [X1,1,Y1,Y2,W1,W2,H1,H2], 
    Vars ins 1..100,
    R1 = rom(X1,W1,Y1,H1),
    R2 = room(1,W2,Y2,H2),
    disjoint2(L),
    labeling([], Vars).
    

    
