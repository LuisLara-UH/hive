:- module(moves, [
    initiate_piece/2, 
    move_piece/2]).

% imports
:- [piece].
:- [moves_utils].
:- [printer].

% piece(type, black or white, Piled, Q, R, S)

% initiate piece
initiate_piece(piece(Type, Color, _, _,  _, _, _), position(Q, R, S)) :-
    \+ position_filled(position(Q, R, S)),
    write("Position not filled.\n"),
    \+ enemy_adjacent(piece(_, Color, _, _, Q, R, S)), 
    write("No enemy piece adjacent.\n"), !,
    is_adjacent(position(Q, R, S), position(Same_Color_Adjacent_Q, Same_Color_Adjacent_R, Same_Color_Adjacent_S)),
    findall_pieces(piece(_,Color, "false",_, Same_Color_Adjacent_Q, Same_Color_Adjacent_R, Same_Color_Adjacent_S), [_|_]),
    write("Found friend piece adjacent.\n"), !,
    add_piece(piece(Type, Color, "false", 0,  Q, R, S)).

% move piece
move_queen(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S)) :- 
    Type = "queen",
    write("Moving queen...\n"),
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    add_piece(piece(Type, Color, Piled, Pile_Number, Next_Q, Next_R, Next_S)),
    write("Queen was moved.\n").

move_beetle(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S)) :- 
    Type = "beetle",
    write("Moving beetle...\n"),
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    (
        (   % no piece in the position
            \+ position_filled(position(Next_Q, Next_R, Next_S)),
            New_Pile_Number is 0
        ); 
        (   % Set unpiled piece as piled
            remove_piece(piece(Next_Type, Next_Color, "false", Piece_Pile_Number, Next_Q, Next_R, Next_S)),
            New_Pile_Number is Piece_Pile_Number + 1,
            add_piece(piece(Next_Type, Next_Color, "true", Piece_Pile_Number, Next_Q, Next_R, Next_S))
        )
    ),
    add_piece(piece(Type, Color, "false", New_Pile_Number, Next_Q, Next_R, Next_S)), 
    write("Beetle was moved.\n"), !,

    % Set last piled piece as unpiled
    Last_Piled_Number is Pile_Number - 1,
    (
        \+ findall_pieces(piece(_, _, "true", Last_Piled_Number, Q, R, S), [piece(Piece_Type, Piece_Color, _,_,_,_,_)|_]);
        remove_piece(piece(Piece_Type, Piece_Color,"true", Last_Piled_Number, Q, R, S)),
        add_piece(piece(Piece_Type, Piece_Color, "false", Last_Piled_Number, Q, R, S))
    ).

move_grasshopper(piece(Type, Color, _, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "grasshopper",
    write("Moving grasshopeer...\n"),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    is_adjacent(position(Q, R, S), position(Adj_Q, Adj_R, Adj_S)),
    position_filled(position(Adj_Q, Adj_R, Adj_S)),
    Q_dir = Q - Adj_Q,
    R_dir = R - Adj_R,
    S_dir = S - Adj_S,
    is_next_blank_inline(position(Adj_Q, Adj_R, Adj_S), position(Q_dir, R_dir, S_dir), position(Next_Q, Next_R, Next_S)),
    add_piece(piece(Type, Color, "false", Pile_Number, Next_Q, Next_R, Next_S)),
    write("Grasshopper was moved.\n").  

move_spider(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "spider". % fill move

move_ant(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ant". % fill move

move_ladybug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ladybug",
    write("Moving ladybug...\n"),
    is_adjacent(position(Q, R, S), position(First_Piece_Q, First_Piece_R, First_Piece_S)),
    position_filled(position(First_Piece_Q, First_Piece_R, First_Piece_S)),
    is_adjacent(position(First_Piece_Q, First_Piece_R, First_Piece_S), position(Second_Piece_Q, Second_Piece_R, Second_Piece_S)),
    position_filled(position(Second_Piece_Q, Second_Piece_R, Second_Piece_S)),
    is_adjacent(position(Second_Piece_Q, Second_Piece_R, Second_Piece_S), position(Next_Q, Next_R, Next_S)),
    (\+ position_filled(position(Next_Q, Next_R, Next_S))),
    add_piece(piece(Type, Color, Piled, Pile_Number, Next_Q, Next_R, Next_S)),
    write("Ladybug was moved.\n").

move_mosquito(piece(Type, Color, _, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "mosquito",
    write("Moving mosquito...\n"),
    (
        (
            (
                position_filled(position(Q, R, S)),
                move_beetle(piece("beetle", Color, "false", Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))
            );
            (
                \+ position_filled(position(Q, R, S)),  
                write("Position not filled.\n"),
                is_adjacent(position(Q, R, S), position(Adjacent_Piece_Q, Adjacent_Piece_R, Adjacent_Piece_S)),
                get_piece(piece(_, _, "false", _, Adjacent_Piece_Q, Adjacent_Piece_R, Adjacent_Piece_S),
                          piece(Adjacent_Type, Color, _, _, _, _, _)),
                \+ Adjacent_Type = "mosquito",
                write("Trying move like "),
                write(Adjacent_Type),
                write("\n"),
                (
                    (
                        add_piece(piece(Adjacent_Type, Color, "false", Pile_Number, Q, R, S)),
                        move_piece(position(Q, R, S), position(Next_Q, Next_R, Next_S))
                    );
                    write("Move mosquito failed.\n"),
                    \+ remove_piece(piece(Adjacent_Type, Color, "false", Pile_Number, Q, R, S))    
                )
            )
        ),
        remove_piece(piece(_, _, "false", New_Pile_Number, Next_Q, Next_R, Next_S)),
        add_piece(piece("mosquito", Color, "false", New_Pile_Number, Next_Q, Next_R, Next_S)),
        write("Mosquito was moved.\n")  
    ).

move_pillbug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "pillbug",
    write("Moving pillbug...\n"),
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    add_piece(piece(Type, Color, Piled, Pile_Number, Next_Q, Next_R, Next_S)),
    write("Pillbug was moved.\n"). 


move_piece(position(Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    remove_piece(piece(Type, Color, "false", Pile_Number, Q, R, S)), !,
    (
        (
            \+ hive_is_divided,
            write("Move does not divide the hive.\n"),
            Piled = "false",
            write("Piece is not piled.\n"),
            \+ (Q is Next_Q, R is Next_R, S is Next_S),
            write("Piece not moves to the same position.\n")
        );
        write("Piece is piled, divides the hive or moves to the same position.\n"),
        \+ add_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S))
    ), !,
    (
        (move_beetle(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_queen(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_grasshopper(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_spider(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_ant(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_ladybug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_mosquito(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_pillbug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S)));
        write("Piece not moved.\n"),
        \+ add_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S))
    ), !,
    (
        \+ (
            hive_is_divided,
            write("Hive was divided.\n"),
            remove_piece(piece(Type, Color, Piled, New_Pile_Number, Next_Q, Next_R, Next_S)),
            add_piece(piece(Type, Color, Piled, New_Pile_Number, Q, R, S)), !,
            fail
        )
    ).