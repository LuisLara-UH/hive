:- [piece].
:- [moves_utils].
:- [moves].

test :-
    initiate_piece(piece("queen", "white", "false", _, _, _, _), position(0, 0, 0)),
    initiate_piece(piece("beetle", "white", "false", _, _, _, _), position(0, -1, 1)),
    initiate_piece(piece("ladybug", "white", "false", _, _, _, _), position(0, 1, -1)),
    initiate_piece(piece("queen", "black", "false", _, _, _, _), position(-1, 1, 0)), !,
    move_piece(position(0, -1, 1), position(0, 0, 0)).