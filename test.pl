:- [piece].
:- [moves_utils].

test :-
    add_piece(piece("queen", "white", "true", 0, 0, 0)),
    add_piece(piece("queen", "white", "true", 0, -1, 1)),
    add_piece(piece("queen", "white", "true", 0, 1, -1)),
    add_piece(piece("queen", "white", "true", -1, 1, 0)),