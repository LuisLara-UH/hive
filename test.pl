:- [piece].
:- [moves_utils].
:- [moves].

:-
    add_piece(piece("queen", "white", "false", 0, 0, 0)),
    add_piece(piece("beetle", "white", "false", 1, -1, 0)),
    add_piece(piece("beetle", "white", "false", 0, 1, -1)),
    add_piece(piece("grasshopper", "white", "false", 1, 0, -1)),
    move_piece(position(1, 0, -1), position(0, 2, -2)).
