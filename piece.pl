:- module(piece, [
    add_piece/1, 
    remove_piece/1, 
    get_pieces/1,
    findall_pieces/2,
    position_filled/1,
    get_piece/2
    ]).

:- dynamic piece/7.

% declares a piece in the database
add_piece(Piece) :- assert(Piece).

% removes a piece from the database
remove_piece(Piece) :- retract(Piece).

% gets the list of pieces declared in the database
get_pieces(Pieces) :- findall(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces).

% gets the list of Pieces with attributes Type, Color, Piled, Pile_Number, Q, R, S
findall_pieces(piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces) :- 
    findall(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces).

% returns true if position Q, R, S is filled
position_filled(position(Q, R, S)) :-
    findall_pieces(piece(_, _, _, _, Q, R, S), [_|_]).

% gets a piece with attributes Type, Color, Piled, Pile_Number, Q, R, S
get_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S), X) :-
    findall_pieces(piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces),
    member(X, Pieces).