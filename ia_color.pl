:- module(ia_color, [
    add_ai/1,
    remove_ai/1,
    ai/1,
    is_ai/1
    ]).

:- dynamic is_ai/1.

% add ai player
add_ai(Color) :- assert(is_ai(Color)).

% remove ai player
remove_ai(Color) :- retract(is_ai(Color)).

% is AI
ai(Color) :-
    findall(_, is_ai(Color), [_|_]).