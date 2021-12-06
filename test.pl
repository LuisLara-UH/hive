:- [piece].
:- [moves_utils].
:- [moves].

:-
    add_piece(piece("queen", "white", "true", 0, 0, 0)),
    add_piece(piece("beetle", "white", "true", 1, -1, 0)),
    add_piece(piece("beetle", "white", "true", 0, 1, -1)),
    add_piece(piece("grasshopper", "white", "true", -1, 0, 1)),
    move_piece(position(-1, 0, 1), position(0, 2, -2)).
