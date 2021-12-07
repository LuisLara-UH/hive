:- module(moves_utils, [
    found_same_color_piece_adjacent/2,
    hive_is_divided/0,
    is_adjacent/2,
    is_next_blank_inline/3,
    move_like_queen_3_times/2
    surround_hive_bfs/2
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
    same_position(position(X1, X2, X3), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q, X2 is R + 1, X3 is S - 1,
    same_position(position(X1, X2, X3), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q + 1, X2 is R - 1, X3 is S,
    same_position(position(X1, X2, X3), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q + 1, X2 is R, X3 is S - 1,
    same_position(position(X1, X2, X3), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q - 1, X2 is R + 1, X3 is S,
    same_position(position(X1, X2, X3), Y).
is_adjacent(position(Q, R, S), Y) :-
    X1 is Q - 1, X2 is R, X3 is S + 1,
    same_position(position(X1, X2, X3), Y).

same_color(Color, Color).


% north
n_neig(piece(_, _, _, _, Q, R, S), X) :- 
    X1 is Q,
    X2 is R - 1,
    X3 is S + 1,
    findall_pieces(piece(_, _, _, _, X1, X2, X3), [X|_]).

% north east
ne_neig(piece(_, _, _, _, Q, R, S), X) :- 
    X1 is Q + 1,
    X2 is R - 1,
    X3 is S,
    findall_pieces(piece(_, _, _, _, X1, X2, X3), [X|_]).

% south east
se_neig(piece(_, _, _, _, Q, R, S), X) :- 
    X1 is Q + 1,
    X2 is R,
    X3 is S - 1,
    findall_pieces(piece(_, _, _, _, X1, X2, X3), [X|_]).

% south
s_neig(piece(_, _, _, _, Q, R, S), X) :- 
    X1 is Q,
    X2 is R + 1,
    X3 is S - 1,
    findall_pieces(piece(_, _, _, _, X1, X2, X3), [X|_]).

% south west
sw_neig(piece(_, _, _, _, Q, R, S), X) :- 
    X1 is Q - 1,
    X2 is R + 1,
    X3 is S,
    findall_pieces(piece(_, _, _, _, X1, X2, X3), [X|_]).

% north west
nw_neig(piece(_, _, _, _, Q, R, S), X) :- 
    X1 is Q - 1,
    X2 is R,
    X3 is S + 1,
    findall_pieces(piece(_, _, _, _, X1, X2, X3), [X|_]).

get_adjacent(position(Q, R, S), position(Q_Adj, R_Adj, S_Adj)) :- 
    n_neig(position(Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    ne_neig(position(Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    se_neig(position(Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    s_neig(position(Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    sw_neig(position(Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    nw_neig(position(Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj)).

move_like_queen_3_times(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Q_New, R_New, S_New)) :-
    % First step
    get_adjacent(piece(Type, Color, Piled, Pile_Number, Q, R, S), piece(_, _, _, _, Q_Adj1, R_Adj1, S_Adj1)),
    \+ position_filled(position(Q_Adj1, R_Adj1, S_Adj1)),

    % Second step
    get_adjacent(piece(Type, Color, Piled, Pile_Number, Q_Adj1, R_Adj1, S_Adj1), piece(_, _, _, _, Q_Adj2, R_Adj2, S_Adj2)),
    \+ position_filled(position(Q_Adj2, R_Adj2, S_Adj2)),

    % Third step
    get_adjacent(piece(Type, Color, Piled, Pile_Number, Q_Adj2, R_Adj2, S_Adj2), piece(_, _, _, _, Q_Adj3, R_Adj3, S_Adj3)),
    \+ position_filled(position(Q_Adj3, R_Adj3, S_Adj3)),

    % Check if it's searched position
    Q_New = Q_Adj3, R_New = R_Adj3, S_New = S_Adj3,

    % Check if final position has a filled adjacent
    get_adjacent(piece(Type, Color, Piled, Pile_Number, Q_Adj3, R_Adj3, S_Adj3), piece(_, _, _, _, Q_Adj4, R_Adj4, S_Adj4)),
    position_filled(position(Q_Adj4, R_Adj4, S_Adj4)).


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

north_dir(Q, R, S) :-
    Q = 0, R = -1, S = 1.

north_east_dir(Q, R, S) :-
    Q = 1, R = -1, S = 0.

south_east_dir(Q, R, S) :-
    Q = 1, R = 0, S = -1.

south_dir(Q, R, S) :-
    Q = 0, R = 1, S = -1.

south_west_dir(Q, R, S) :-
    Q = -1, R = 1, S = 0.

north_west_dir(Q, R, S) :-
    Q = -1, R = 0, S = 1.

% grasshopper: searches for the next blank position in a direction
is_next_blank_inline(Y, _, Y).
is_next_blank_inline(position(Q, R, S), position(Q_dir, R_dir, S_dir), position(New_Q, New_R, New_S)) :-
    write("blank inline"),
    position_filled(position(Q, R, S)),
    X1 is Q - Q_dir,
    X2 is R - R_dir,
    X3 is S - S_dir,
    is_next_blank_inline(position(X1, X2, X3), position(Q_dir, R_dir, S_dir), position(New_Q, New_R, New_S)).


is_adjacent_to_filled_piece(piece(_, _, _, _, Q, R, S)) :-
    get_adjacent(position(Q, R, S), position(Q_Adj, R_Adj, S_Adj)),
    position_filled(position(Q_Adj, R_Adj, S_Adj)).


find_blank_path([final_pos(Q, R, S), _], _, final_pos(Q, R, S)).
find_blank_path([Piece|Non_Visited_Pieces], Pieces_Found, final_pos(Q, R, S)) :-
    (
        n_neig(Piece, Next_Piece),
        \+ member(Next_Piece, Pieces_Found),
        is_adjacent_to_filled_piece(Next_Piece),
        append(Pieces_Found, [Next_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Next_Piece], New_Non_Visited_Pieces),
        find_blank_path([Piece|New_Non_Visited_Pieces], New_Pieces_Found, final_pos(Q, R, S))        
    );
    (
        ne_neig(Piece, Next_Piece),
        \+ member(Next_Piece, Pieces_Found),
        is_adjacent_to_filled_piece(Next_Piece),
        append(Pieces_Found, [Next_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Next_Piece], New_Non_Visited_Pieces),
        find_blank_path([Piece|New_Non_Visited_Pieces], New_Pieces_Found, final_pos(Q, R, S)) 
    );
    (
        se_neig(Piece, Next_Piece),
        \+ member(Next_Piece, Pieces_Found),
        is_adjacent_to_filled_piece(Next_Piece),
        append(Pieces_Found, [Next_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Next_Piece], New_Non_Visited_Pieces),
        find_blank_path([Piece|New_Non_Visited_Pieces], New_Pieces_Found, final_pos(Q, R, S)) 
    );
    (
        s_neig(Piece, Next_Piece),
        \+ member(Next_Piece, Pieces_Found),
        is_adjacent_to_filled_piece(Next_Piece),
        append(Pieces_Found, [Next_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Next_Piece], New_Non_Visited_Pieces),
        find_blank_path([Piece|New_Non_Visited_Pieces], New_Pieces_Found, final_pos(Q, R, S)) 
    );
    (
        sw_neig(Piece, Next_Piece),
        \+ member(Next_Piece, Pieces_Found),
        is_adjacent_to_filled_piece(Next_Piece),
        append(Pieces_Found, [Next_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Next_Piece], New_Non_Visited_Pieces),
        find_blank_path([Piece|New_Non_Visited_Pieces], New_Pieces_Found, final_pos(Q, R, S))
    );
    (
        nw_neig(Piece, Next_Piece),
        \+ member(Next_Piece, Pieces_Found),
        is_adjacent_to_filled_piece(Next_Piece),
        append(Pieces_Found, [Next_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Next_Piece], New_Non_Visited_Pieces),
        find_blank_path([Piece|New_Non_Visited_Pieces], New_Pieces_Found, final_pos(Q, R, S)) 
    );
    find_blank_path(Non_Visited_Pieces, Pieces_Found, final_pos(Q, R, S)).


surround_hive_bfs(current_pos(Q, R, S), final_pos(Q, R, S)) :-
    find_blank_path([piece(_, _, _, _, Q, R, S)], [piece(_, _, _, _, Q, R, S)], final_pos(Q, R, S)).