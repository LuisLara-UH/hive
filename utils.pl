:- module(utils, [:- op(700, fx, found_same_color_piece_adjacent)]).

:- [piece].

:- op(700, fx, is_adjacent).
:- op(700, fx, same_position).
:- op(700, fx, same_color).
:- op(700, fx, divides_hive).
:- op(700, fx, has_same_color_adjacent).


% piece(type, black or white, Piled, Q, R, S)
found_same_color_piece_adjacent Color, Position :-
    get_pieces Pieces,
    has_same_color_adjacent Color, Position, Pieces.


has_same_color_adjacent Color1, Piece_Position, [piece(_, Color2, _, Q, R, S)|Y] :-
    is_adjacent Piece_Position, position(Q, R, S), same_color Color1, Color2;
    has_same_color_adjacent(Color1, Piece_Position, Y).


same_position position(Q, R, S), position(Q, R, S).


is_adjacent position(Q, R, S), Y :-
    same_position(position(Q, R - 1, S + 1), Y);
    same_position(position(Q, R + 1, S - 1), Y);
    same_position(position(Q + 1, R - 1, S), Y);
    same_position(position(Q + 1, R, S - 1), Y);
    same_position(position(Q - 1, R + 1, S), Y);
    same_position(position(Q - 1, R, S + 1), Y).

same_color Color, Color.

divides_hive position(Q, R, S)