:- module(moves_utils, [
    found_same_color_piece_adjacent/2,
    hive_is_divided/0,
    is_adjacent/2
    ]).

:- [piece].

% piece(type, black or white, Piled, Q, R, S)
found_same_color_piece_adjacent(Color, Position) :-
    get_pieces(Pieces),
    has_same_color_adjacent(Color, Position, Pieces).


has_same_color_adjacent(Color1, Piece_Position, [piece(_, Color2, _, Q, R, S)|Y]) :-
    is_adjacent(Piece_Position, position(Q, R, S)), 
    same_color(Color1, Color2);
    has_same_color_adjacent(Color1, Piece_Position, Y).


same_position(position(Q, R, S), position(Q, R, S)).


is_adjacent(position(Q, R, S), Y) :-
    X1 is Q, X2 is R - 1, X3 is S + 1,
    same_position(position(Q, R - 1, S + 1), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q, X2 is R + 1, X3 is S - 1,
    same_position(position(Q, R + 1, S - 1), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q + 1, X2 is R - 1, X3 is S,
    same_position(position(Q + 1, R - 1, S), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q + 1, X2 is R, X3 is S - 1,
    same_position(position(Q + 1, R, S - 1), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q - 1, X2 is R + 1, X3 is S,
    same_position(position(Q - 1, R + 1, S), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q - 1, X2 is R, X3 is S + 1,
    same_position(position(Q - 1, R, S + 1), Y).

same_color(Color, Color).


% north
n_neig(piece(_, _, _, Q, R, S), X) :- 
    X1 is Q,
    X2 is R - 1,
    X3 is S + 1,
    findall_pieces(piece(_, _, _, X1, X2, X3), [X|_]).

% north east
ne_neig(piece(_, _, _, Q, R, S), X) :- 
    X1 is Q + 1,
    X2 is R - 1,
    X3 is S,
    findall_pieces(piece(_, _, _, X1, X2, X3), [X|_]).

% south east
se_neig(piece(_, _, _, Q, R, S), X) :- 
    X1 is Q + 1,
    X2 is R,
    X3 is S - 1,
    findall_pieces(piece(_, _, _, X1, X2, X3), [X|_]).

% south
s_neig(piece(_, _, _, Q, R, S), X) :- 
    X1 is Q,
    X2 is R + 1,
    X3 is S - 1,
    findall_pieces(piece(_, _, _, X1, X2, X3), [X|_]).

% south west
sw_neig(piece(_, _, _, Q, R, S), X) :- 
    X1 is Q - 1,
    X2 is R + 1,
    X3 is S,
    findall_pieces(piece(_, _, _, X1, X2, X3), [X|_]).

% north west
nw_neig(piece(_, _, _, Q, R, S), X) :- 
    X1 is Q - 1,
    X2 is R,
    X3 is S + 1,
    findall_pieces(piece(_, _, _, X1, X2, X3), [X|_]).


find_pieces_connected([], Pieces_Found, Connected_Pieces) :- append([], Pieces_Found, Connected_Pieces).
find_pieces_connected([Piece|Non_Visited_Pieces], Pieces_Found, Connected_Pieces) :-
    (
        n_neig(Piece, North_Piece),
        \+ member(North_Piece, Pieces_Found),
        append(Pieces_Found, [North_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [North_Piece], New_Non_Visited_Pieces),
        find_pieces_connected([Piece|New_Non_Visited_Pieces], New_Pieces_Found, Connected_Pieces)
    );
    (
        ne_neig(Piece, North_East_Piece),
        \+ member(North_East_Piece, Pieces_Found),
        append(Pieces_Found, [North_East_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [North_East_Piece], New_Non_Visited_Pieces),
        find_pieces_connected([Piece|New_Non_Visited_Pieces], New_Pieces_Found, Connected_Pieces)
    );
    (
        se_neig(Piece, South_East_Piece),
        \+ member(South_East_Piece, Pieces_Found),
        append(Pieces_Found, [South_East_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [South_East_Piece], New_Non_Visited_Pieces),
        find_pieces_connected([Piece|New_Non_Visited_Pieces], New_Pieces_Found, Connected_Pieces)
    );
    (
        s_neig(Piece, South_Piece),
        \+ member(South_Piece, Pieces_Found),
        append(Pieces_Found, [South_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [South_Piece], New_Non_Visited_Pieces),
        find_pieces_connected([Piece|New_Non_Visited_Pieces], New_Pieces_Found, Connected_Pieces)
    );
    (
        sw_neig(Piece, South_West_Piece),
        \+ member(South_West_Piece, Pieces_Found),
        append(Pieces_Found, [South_West_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [South_West_Piece], New_Non_Visited_Pieces),
        find_pieces_connected([Piece|New_Non_Visited_Pieces], New_Pieces_Found, Connected_Pieces)
    );
    (
        nw_neig(Piece, North_West_Piece),
        \+ member(North_West_Piece, Pieces_Found),
        append(Pieces_Found, [North_West_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [North_West_Piece], New_Non_Visited_Pieces),
        find_pieces_connected([Piece|New_Non_Visited_Pieces], New_Pieces_Found, Connected_Pieces)
    );
    find_pieces_connected(Non_Visited_Pieces, Pieces_Found, Connected_Pieces).

hive_is_divided :- 
    get_pieces([Piece|Other_Pieces]),
    find_pieces_connected([Piece], [Piece], Connected_Pieces),
    length([Piece|Other_Pieces], Pieces_Len),
    length(Connected_Pieces, Connected_Pieces_Len), !,
    \+ Pieces_Len is Connected_Pieces_Len. 