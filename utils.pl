:- [piece].

same_color_piece_adjacent(Color, Row, Column) :-
    get_pieces Pieces,
    has_adjacent(position(Row, Column), Pieces).

has_adjacent(Piece_Position, [piece(_, _, _, Row, Column)|Y]) :-
    is_adjacent(Piece_Position, position(Row, Column));
    has_adjacent(Piece_Position, Y).

same_position(position(Row, Column), position(Row, Column)).

is_adjacent(position(Row1, Column1), Y):-
    same_position(position(Row1 + 1, Column1), Y);
    same_position(position(Row1, Column1 + 1), Y).