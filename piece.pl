:- module(piece, [
    add_piece/1, 
    add_pieces/1,
    remove_piece/1, 
    remove_pieces/0,
    get_pieces/1,
    findall_pieces/2,
    position_filled/1,
    get_piece/2,
    restart_game/1,
    count_pieces/2
    ]).

:- dynamic piece/7.

% declares a piece in the database
add_piece(Piece) :- assert(Piece).

add_pieces([]).
add_pieces([Piece|Other_Pieces]) :-
    add_piece(Piece),
    add_pieces(Other_Pieces).

% removes a piece from the database
remove_piece(Piece) :- retract(Piece).

% remove all pieces
remove_pieces :-
    retractall(piece(_,_,_,_,_,_,_)).

% gets the list of pieces declared in the database
get_pieces(Pieces) :- findall(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces).

% gets the list of Pieces with attributes Type, Color, Piled, Pile_Number, Q, R, S
findall_pieces(piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces) :- 
    findall(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces).

% returns true if position Q, R, S is filled
position_filled(position(Q, R, S)) :-
    findall_pieces(piece(_, _, _, _, Q, R, S), Pieces),
    member(piece(_,_,_,_,Piece_Q, Piece_R, Piece_S), Pieces),
    Q is Piece_Q, R is Piece_R, S is Piece_S.

% gets a piece with attributes Type, Color, Piled, Pile_Number, Q, R, S
get_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S), X) :-
    findall_pieces(piece(Type, Color, Piled, Pile_Number, Q, R, S), Pieces),
    member(X, Pieces).

% re-start game position
restart_game(Pos) :-
    remove_pieces,
    add_pieces(Pos).

% count amount of pieces
count_pieces(piece(Type, Color,_,_,_,_,_), Amount) :-
    findall_pieces(piece(Type, Color,_,_,_,_,_), Pieces),
    length(Pieces, Amount), !.

count_pieces(piece(_,_,_,_,_,_,_), 0).