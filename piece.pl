:- module(piece, [
    add_piece/1, 
    remove_piece/1, 
    get_pieces/1,
    findall_pieces/2
    ]).

:- dynamic piece/6.

% declares a piece in the database
add_piece(Piece) :- assert(Piece).

% removes a piece from the database
remove_piece(Piece) :- retract(Piece).

% gets the list of pieces declared in the database
get_pieces(Pieces) :- findall(piece(Type, Color, Piled, Q, R, S), piece(Type, Color, Piled, Q, R, S), Pieces).

findall_pieces(piece(Type, Color, Piled, Q, R, S), Pieces) :- 
    findall(piece(Type, Color, Piled, Q, R, S), piece(Type, Color, Piled, Q, R, S), Pieces).