:- [piece].
:- [moves_utils].
:- [moves].

test :-
    add_piece(piece("queen", "white", "false", 0, 0, 0, 0)),
    add_piece(piece("beetle", "white", "false", 0, 0, -1, 1)),
    add_piece(piece("ladybug", "white", "false", 0, 0, 1, -1)),
    add_piece(piece("queen", "black", "false", 0, -1, 1, 0)),
    move_piece(position(0, -1, 1), position(0, 0, 0)).