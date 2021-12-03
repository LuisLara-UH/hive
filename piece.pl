:- module(piece, [op(700, fx, add_piece), add_piece/1, 
    op(700, fx, remove_piece), remove_piece/1, 
    op(700, fx, get_pieces), get_pieces/1]).

:- dynamic piece/5.

% declares a piece in the database
add_piece Piece :- assert(Piece).

% removes a piece from the database
remove_piece Piece :- retract(Piece).

% gets the list of pieces declared in the database
get_pieces Pieces :- findall(piece(Type, Color, Piled, Row, Column), _, Pieces).