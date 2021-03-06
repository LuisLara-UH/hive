:- module(utils, [
    find_next_positions/3,
    board_value/2
    ]).

:- [piece].
:- [moves].
:- [moves_utils].
:- [turn].
:- [printer].

is_subset([], _).
is_subset([Piece1|Other_Pieces], Set1) :-
    member(Piece1, Set1),
    is_subset(Other_Pieces, Set1).

same_board(Pos1, Pos2) :-
    is_subset(Pos1, Pos2),
    is_subset(Pos2, Pos1).

has_position(Pos_List, Pos) :-
    member(Pos1, Pos_List),
    same_board(Pos, Pos1).

find_position(Actual_Pos, Next_Pos) :-
    turn_color(Color),
    next_position(Color),
    get_pieces(Next_Pos),
    restart_game(Actual_Pos).

next_position(Color) :-
    is_valid_blank_position(position(Q, R, S)),
    initiate_piece(piece(_, Color, _,_,_,_,_), position(Q, R, S)).

next_position(Color) :-
    get_piece(piece(_, Color, _,_,_,_,_), piece(_,_,_,_, Piece_Q, Piece_R, Piece_S)),
    (
        is_valid_blank_position(position(Q, R, S));
        position_filled(position(Q, R, S))
    ),
    move_piece(position(Piece_Q, Piece_R, Piece_S), position(Q, R, S)).

find_next_positions(Pos, Pos_Found_List, Pos_List) :-
    restart_game(Pos),
    find_position(Pos, Pos_Found),
    \+ same_board(Pos, Pos_Found),
    \+ has_position(Pos_Found_List, Pos_Found),
    append(Pos_Found_List, [Pos_Found], New_Pos_Found_List),
    find_next_positions(Pos, New_Pos_Found_List, Pos_List).

find_next_positions(_, Pos_List, Pos_List).

board_value(Pos, Val) :-
    find_next_positions(Pos, [], Pos_List),
    length(Pos_List, Val1),
    turn_color(Color),

    (   
        findall_pieces(piece("queen", Color, _,_,_,_,_), [piece(_, _, _,_, Q, R, S)|_]),
        adjacent_pieces_amount(piece("queen", Color, _,_, Q, R, S), Val2);
        Val2 is 0
    ),
    (
        enemy_color(Color, EnemyColor),
        findall_pieces(piece("queen", EnemyColor, _,_,_,_,_), [piece(_, _, _,_, Enemy_Q, Enemy_R, Enemy_S)|_]),
        adjacent_pieces_amount(piece("queen", EnemyColor, _,_, Enemy_Q, Enemy_R, Enemy_S), Val3);
        Val3 is 0
    ),
    
    Val is Val1 + Val2 - Val3.