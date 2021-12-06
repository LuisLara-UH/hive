:- module(moves, [
    initiate_piece/2, 
    move_piece/2]).

% imports
:- [piece].
:- [moves_utils].

% piece(type, black or white, Piled, Q, R, S)

% initiate piece
initiate_piece(piece(Type, Color, _, _,  _, _, _), position(Q, R, S)) :-
    \+ position_filled(position(Q, R, S)),
    (
        \+ (
        is_adjacent(position(Q, R, S), position(Adjacent_Q, Adjacent_R, Adjacent_S)),
        \+ Adjacent_Color is Color,
        findall_pieces(piece(_,Adjacent_Color, "false",_, Adjacent_Q, Adjacent_R, Adjacent_S), [_|_])
       )
    ), !,
    is_adjacent(position(Q, R, S), position(Same_Color_Adjacent_Q, Same_Color_Adjacent_R, Same_Color_Adjacent_S)),
    findall_pieces(piece(_,Color, "false",_, Adjacent_Q, Adjacent_R, Adjacent_S), [_|_]),
    add_piece(piece(Type, Color, "false", 0,  Q, R, S)).

% move piece
move_queen(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S)) :- 
    Type = "queen",
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    add_piece(piece(Type, Color, Piled, Pile_Number, Next_Q, Next_R, Next_S)).

move_beetle(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S)) :- 
    Type = "beetle",
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
    add_piece(piece(Type, Color, Piled, New_Pile_Number, Next_Q, Next_R, Next_S)), !,
    % Set last piled piece as unpiled
    Last_Piled_Number is Pile_Number - 1,
    findall_pieces(piece(_, _, "true", Last_Piled_Number, Q, R, S), [piece(Piece_Type, Piece_Color, _,_,_,_,_)|_]),
    remove_piece(piece(Piece_Type, Piece_Color,"true", Last_Piled_Number, Q, R, S)),
    add_piece(piece(Piece_Type, Piece_Color, "false", Last_Piled_Number, Q, R, S)).

move_grasshopper(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "grasshopper",
    \+ position_filled(position(Next_Q, Next_R, Next_S)); % no piece in the position
    is_adjacent(position(Q, R, S), position(Adj_Q, Adj_R, Adj_S)),
    position_filled(position(Adj_Q, Adj_R, Adj_S)),
    Q_dir = Q - Adj_Q,
    R_dir = R - Adj_R,
    S_dir = S - Adj_S,
    is_next_blank_inline(position(Adj_Q, Adj_R, Adj_S), position(Q_dir, R_dir, S_dir), position(Next_Q, Next_R, Next_S)).  

move_spider(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "spider". % fill move

move_ant(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ant". % fill move

move_ladybug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ladybug",
    is_adjacent(position(Q, R, S), position(first_piece_Q, first_piece_R, first_piece_S)),
    position_filled(position(first_piece_Q, first_piece_R, first_piece_S)),
    is_adjacent(position(first_piece_Q, first_piece_R, first_piece_S), position(second_piece_Q, second_piece_R, second_piece_S)),
    position_filled(position(second_piece_Q, second_piece_R, second_piece_S)),
    is_adjacent(position(second_piece_Q, second_piece_R, second_piece_S), position(blank_space_Q, blank_space_R, blank_space_S)),
    \+ position_filled(position(blank_space_Q, blank_space_R, blank_space_S)),
    add_piece(piece(Type, Color, Piled, Pile_Number, Next_Q, Next_R, Next_S)). % fill move

move_mosquito(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "mosquito",
    (
        (
            (
                position_filled(position(Q, R, S)),
                move_beetle(piece("beetle", Color, "false", Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))
            );
            (
                \+ position_filled(position(Q, R, S)),  
                is_adjacent(position(Q, R, S), position(adjacent_piece_Q, adjacent_piece_R, adjacent_piece_S)),
                findall_pieces(piece(Adjacent_Type, _, "false", _, adjacent_piece_Q, adjacent_piece_R, adjacent_piece_S), [adjacent_piece|_]),
                \+ Adjacent_Type = "mosquito",
                (
                    (
                        add_piece(piece(Adjacent_Type, Color, "false", Pile_Number, Q, R, S)),
                        move_piece(position(Q, R, S), position(Next_Q, Next_R, Next_S))
                    );
                    \+ remove_piece(piece(Adjacent_Type, Color, "false", Pile_Number, Q, R, S))    
                )
            )
        ),
        remove_piece(piece(_, _, "false", New_Pile_Number, Next_Q, Next_R, Next_S)),
        add_piece(piece("mosquito", Color, "false", New_Pile_Number, Next_Q, Next_R, Next_S))   
    ).

move_pillbug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "pillbug",
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    add_piece(piece(Type, Color, Piled, Pile_Number, Next_Q, Next_R, Next_S)). 


move_piece(position(Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    remove_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S)), !,
    (
        (
            \+ hive_is_divided,
            Piled = "false"
        );
        \+ add_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S))
    ), !,
    (
        move_beetle(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_queen(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_grasshopper(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_spider(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_ant(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_ladybug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_mosquito(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        move_pillbug(piece(Type, Color, Piled, Pile_Number, Q, R, S), position(Next_Q, Next_R, Next_S));
        \+ add_piece(piece(Type, Color, Piled, Pile_Number, Q, R, S))
    ), !,
    (
        \+ hive_is_divided;
        (
            remove_piece(piece(Type, Color, Piled, New_Pile_Number, Next_Q, Next_R, Next_S)),
            add_piece(piece(Type, Color, Piled, New_Pile_Number, Q, R, S)), !,
            fail
        )
    ).