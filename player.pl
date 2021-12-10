:- module(player, [
    minimax/5,
    alphabeta/7
    ]).

:- [piece].
:- [utils].
:- [turn].
:- [printer].

minimax(Pos, BestSucc, Val, Depth, Player_Color) :-
    moves(Pos, PosList),
    !,
    Next_Depth is Depth - 1,
    (
        (
            Depth > 0,
            change_turn,
            best(PosList, BestSucc, Val, Next_Depth, Player_Color),
            change_turn
        );
        (
            staticval(Pos, Val, Player_Color)
        )
    ).

best([Pos], Pos, Val, Depth, Player_Color) :-
    minimax(Pos, _, Val, Depth, Player_Color), !.

best([Pos1 | PosList], BestPos, BestVal, Depth, Player_Color) :-
    minimax(Pos1, _, Val1, Depth, Player_Color),
    best(PosList, Pos2, Yal2, Depth, Player_Color),
    betterof(Pos1, Val1, Pos2, Yal2, BestPos, BestVal, Player_Color).

betterof(Pos0, Val0, Pos1, Val1, Pos0, Val0, Player_Color) :-
    min_value(Pos0, Player_Color), Val0 > Val1, !;
    max_value(Pos0, Player_Color), Val0 < Val1, !.

betterof(PosO, Val0, Pos1, Val1, Pos1, Val1, Player_Color).

moves(Pos, PosList) :- 
    find_next_positions(Pos, [], PosList).

max_value(Pos, Player_Color) :-
    last(Pos, piece(_, Player_Color, _,_,_,_,_)), !.

min_value(Pos, Player_Color) :-
    \+ last(Pos, piece(_, Player_Color, _,_,_,_,_)), !.

staticval(Pos, Val, Player_Color) :- 
    turn_color(Player_Color), !,
    change_turn,
    board_value(Pos, Val),
    change_turn, !.

staticval(Pos, Val, Player_Color) :-
    board_value(Pos, Val).

alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth, Player_Color) :-
    moves(Pos, Poslist), !,
    Next_Depth = Depth - 1,
    (
        (
            Depth > 0,
            change_turn,
            boundedbest(Poslist, Alpha, Beta, GoodPos, Val, Next_Depth, Player_Color),
            change_turn
        );
        (
            staticval(Pos, Val, Player_Color)
        )
    ).

boundedbest([Pos|Poslist], Alpha, Beta, GoodPos, GoodVal, Depth, Player_Color) :-
    alphabeta( Pos, Alpha, Beta, _, Val, Depth, Player_Color),
    goodenough( Poslist, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth, Player_Color).

goodenough([], _, _, Pos, Val, Pos, Val, Depth, Player_Color) :- !.

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, Depth, Player_Color) :- 
    min_value(Pos, Player_Color), Val > Beta, !;
    max_value(Pos, Player_Color), Val < Alpha, !.

goodenough(Poslist, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth, Player_Color) :-
    newbounds( Alpha, Beta, Pos, Val, NewAlpha, NewBeta, Player_Color),
    boundedbest( Poslist, NewAlpha, NewBeta, Pos1, Val1, Depth, Player_Color),
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal, Player_Color).

newbounds(Alpha, Beta, Pos, Val, Val, Beta, Player_Color) :-
    min_value(Pos, Player_Color), Val > Alpha, !.

newbounds(Alpha, Beta, Pos, Val, Alpha, Val, Player_Color) :-
    max_value(Pos, Player_Color), Val < Beta, !.

newbounds(Alpha, Beta, _, _, Alpha, Beta, Player_Color).