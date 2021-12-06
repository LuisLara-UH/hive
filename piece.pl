:- module(piece, [
    add_piece/1, 
    remove_piece/1, 
    get_pieces/1,
    findall_pieces/2,
    position_filled/1
    ]).

:- dynamic piece/7.

% declares a piece in the database
add_piece(Piece) :- assert(Piece).

% removes a piece from the database
remove_piece(Piece) :- retract(Piece).

% gets the list of pieces declared in the database
get_pieces(Pieces) :- findall(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces).

findall_pieces(piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces) :- 
    findall(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces).

position_filled(position(Q, R, S)) :-
    findall_pieces(piece(_, _, _, _, Q, R, S), [_|_]).