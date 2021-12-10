:- module(moves_utils, [
    found_same_color_piece_adjacent/2,
    hive_is_divided/0,
    is_adjacent/2,
    is_next_blank_inline/3,
    surround_hive_bfs/2,
    enemy_adjacent/1,
    is_valid_blank_position/1,
    can_init_piece/1,
    same_position/2
    ]).

:- [piece].

% piece(type, black or white, Piled, Pile_Number, Q, R, S)
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "queen", count_pieces(piece(Type, Color,_,_,_,_,_), Queens), Queens < 1.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "grasshopper", count_pieces(piece(Type, Color,_,_,_,_,_), Grasshoppers), Grasshoppers < 3.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "beetle", count_pieces(piece(Type, Color,_,_,_,_,_), Beetles), Beetles < 2.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "spider", count_pieces(piece(Type, Color,_,_,_,_,_), Spiders), Spiders < 2.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "ant", count_pieces(piece(Type, Color,_,_,_,_,_), Ants), Ants < 3.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "mosquito", count_pieces(piece(Type, Color,_,_,_,_,_), Mosquitos), Mosquitos < 1.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "ladybug", count_pieces(piece(Type, Color,_,_,_,_,_), Ladybugs), Ladybugs < 1.
can_init_piece(piece(Type, Color, _,_,_,_,_)) :-
    Type = "pillbug", count_pieces(piece(Type, Color,_,_,_,_,_), Pillbugs), Pillbugs < 1.

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

enemy_adjacent(piece(_, Color, _, _, Q, R, S)) :-
    (
      (Color = "white", Adjacent_Color = "black");  
      (Color = "black", Adjacent_Color = "white")
    ),
    is_adjacent(position(Q, R, S), position(Adjacent_Q, Adjacent_R, Adjacent_S)),
    findall_pieces(piece(_, Adjacent_Color, "false", _, Adjacent_Q, Adjacent_R, Adjacent_S), [_|_]).


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

% same position(Piled)
p_neig(piece(_, _, _, _, Q, R, S), X) :-
    findall_pieces(piece(_, _, _, _, Q, R, S), Pieces),
    member(X, Pieces).

get_adjacent(position(Q, R, S), position(Q_Adj, R_Adj, S_Adj)) :- 
    n_neig(piece(_, _, _, _, Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    ne_neig(piece(_, _, _, _, Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    se_neig(piece(_, _, _, _, Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    s_neig(piece(_, _, _, _, Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    sw_neig(piece(_, _, _, _, Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj));
    nw_neig(piece(_, _, _, _, Q, R, S), piece(_, _, _, _, Q_Adj, R_Adj, S_Adj)).


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
    (
        p_neig(Piece, Piled_Piece),
        \+ member(Piled_Piece, Pieces_Found),
        append(Pieces_Found, [Piled_Piece], New_Pieces_Found),
        append(Non_Visited_Pieces, [Piled_Piece], New_Non_Visited_Pieces),
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
    position_filled(position(Q, R, S)),
    X1 is Q - Q_dir,
    X2 is R - R_dir,
    X3 is S - S_dir,
    is_next_blank_inline(position(X1, X2, X3), position(Q_dir, R_dir, S_dir), position(New_Q, New_R, New_S)).


is_valid_blank_position(position(Q, R, S)) :-
    position_filled(position(Q_Adj, R_Adj, S_Adj)),
    is_adjacent(position(Q_Adj, R_Adj, S_Adj), position(Q, R, S)),
    \+ position_filled(position(Q, R, S)).


find_blank_path([position(Q, R, S)|_], _, position(Q, R, S)).
find_blank_path([position(Q, R, S) | Non_Visited_Positions], Positions_Found, position(Final_Q, Final_R, Final_S)) :-
    (
        North_Space_Q is Q, North_Space_R is R - 1, North_Space_S is S + 1, 
        North_Position = position(North_Space_Q, North_Space_R, North_Space_S),
        \+ member(North_Position, Positions_Found),
        is_valid_blank_position(North_Position),
        append(Positions_Found, [North_Position], New_Positions_Found),
        append(Non_Visited_Positions, [North_Position], New_Non_Visited_Positions),
        find_blank_path([position(Q, R, S) | New_Non_Visited_Positions], New_Positions_Found, position(Final_Q, Final_R, Final_S))        
    );
    (
        North_East_Space_Q is Q + 1, North_East_Space_R is R - 1, North_East_Space_S is S, 
        North_East_Position = position(North_East_Space_Q, North_East_Space_R, North_East_Space_S),
        \+ member(North_East_Position, Positions_Found),
        is_valid_blank_position(North_East_Position),
        append(Positions_Found, [North_East_Position], New_Positions_Found),
        append(Non_Visited_Positions, [North_East_Position], New_Non_Visited_Positions),
        find_blank_path([position(Q, R, S) | New_Non_Visited_Positions], New_Positions_Found, position(Final_Q, Final_R, Final_S))  
    );
    (
        South_East_Space_Q is Q + 1, South_East_Space_R is R, South_East_Space_S is S - 1, 
        South_East_Position = position(South_East_Space_Q, South_East_Space_R, South_East_Space_S),
        \+ member(South_East_Position, Positions_Found),
        is_valid_blank_position(South_East_Position),
        append(Positions_Found, [South_East_Position], New_Positions_Found),
        append(Non_Visited_Positions, [South_East_Position], New_Non_Visited_Positions),
        find_blank_path([position(Q, R, S) | New_Non_Visited_Positions], New_Positions_Found, position(Final_Q, Final_R, Final_S))  
    );
    (
        South_Space_Q is Q, South_Space_R is R + 1, South_Space_S is S - 1, 
        South_Position = position(South_Space_Q, South_Space_R, South_Space_S),
        \+ member(South_Position, Positions_Found),
        is_valid_blank_position(South_Position),
        append(Positions_Found, [South_Position], New_Positions_Found),
        append(Non_Visited_Positions, [South_Position], New_Non_Visited_Positions),
        find_blank_path([position(Q, R, S) | New_Non_Visited_Positions], New_Positions_Found, position(Final_Q, Final_R, Final_S)) 
    );
    (
        South_West_Space_Q is Q - 1, South_West_Space_R is R + 1, South_West_Space_S is S, 
        South_West_Position = position(South_West_Space_Q, South_West_Space_R, South_West_Space_S),
        \+ member(South_West_Position, Positions_Found),
        is_valid_blank_position(South_West_Position),
        append(Positions_Found, [South_West_Position], New_Positions_Found),
        append(Non_Visited_Positions, [South_West_Position], New_Non_Visited_Positions),
        find_blank_path([position(Q, R, S) | New_Non_Visited_Positions], New_Positions_Found, position(Final_Q, Final_R, Final_S)) 
    );
    (
        North_West_Space_Q is Q - 1, North_West_Space_R is R, North_West_Space_S is S + 1, 
        North_West_Position = position(North_West_Space_Q, North_West_Space_R, North_West_Space_S),
        \+ member(North_West_Position, Positions_Found),
        is_valid_blank_position(North_West_Position),
        append(Positions_Found, [North_West_Position], New_Positions_Found),
        append(Non_Visited_Positions, [North_West_Position], New_Non_Visited_Positions),
        find_blank_path([position(Q, R, S) | New_Non_Visited_Positions], New_Positions_Found, position(Final_Q, Final_R, Final_S)) 
    );
    find_blank_path(Non_Visited_Positions, Positions_Found, position(Final_Q, Final_R, Final_S)).


surround_hive_bfs(position(Q, R, S), position(Q_Final, R_Final, S_Final)) :-
    find_blank_path([position(Q, R, S)], [position(Q, R, S)], position(Q_Final, R_Final, S_Final)).