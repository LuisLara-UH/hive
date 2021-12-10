:- module(turn, [
    change_turn/0,
    turn_color/1
    ]).

:- dynamic black_turn/0.

change_turn :-
    retract(black_turn), !.

change_turn :-
    assert(black_turn).

turn_color(Color) :-
    black_turn, Color = "black", !. 

turn_color(Color) :-
    Color = "white".