:- [piece].
:- [moves_utils].

test :-
    add_piece(piece("queen", "white", "false", 0, 0, 0, 0)),
    add_piece(piece("queen", "white", "false", 0, 0, -1, 1)),
    add_piece(piece("queen", "white", "false", 0, 0, 1, -1)),
    add_piece(piece("queen", "white", "false", 0, -1, 1, 0)).