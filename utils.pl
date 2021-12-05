:- [piece].

:- op(700, fx, is_adjacent).
:- op(700, fx, same_position).
:- op(700, fx, same_color).
:- op(700, fx, found_same_color_piece_adjacent).
:- op(700, fx, has_same_color_adjacent).


% piece(type, black or white, Piled, Row, Column)
found_same_color_piece_adjacent Color, Position :-
    get_pieces Pieces,
    has_same_color_adjacent Color, Position, Pieces.


has_same_color_adjacent Color1, Piece_Position, [piece(_, Color2, _, Row, Column)|Y] :-
    is_adjacent Piece_Position, position(Row, Column), same_color Color1, Color2;
    has_same_color_adjacent(Color1, Piece_Position, Y).


same_position position(Row, Column), position(Row, Column).


is_adjacent position(Row, Column), Y :-
    same_position(position(Row + 1, Column), Y);
    same_position(position(Row, Column + 1), Y);
    same_position(position(Row - 1, Column, Y));
    same_position(position(Row, Column - 1), Y).

same_color Color, Color.