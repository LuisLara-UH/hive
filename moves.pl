:- module(moves, [
    initiate_piece/2, 
    move_piece/2]).

% imports
:- [piece].
:- [moves_utils].

% piece(type, black or white, Piled, Q, R, S)

% initiate piece
initiate_piece(piece(Type, Color, _,  _, _, _), position(Q, R, S)) :- !. % fill initiate

% move piece
move_queen(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S)) :- 
    Type = "queen",
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    add_piece(piece(Type, Color, Piled, Next_Q, Next_R, Next_S)).

move_beetle(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S)) :- 
    Type = "beetle",
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    (
        \+ position_filled(position(Next_Q, Next_R, Next_S)); % no piece in the position
        (   % Set unpiled piece as piled
            remove_piece(piece(Type, Color, "false", Next_Q, Next_R, Next_S)),
            add_piece(piece(Type, Color, "true", Next_Q, Next_R, Next_S))
        )
    ),
    add_piece(piece(Type, Color, Piled, Next_Q, Next_R, Next_S)).

move_grasshopper(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "grasshopper". % fill move

move_spider(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "spider". % fill move

move_ant(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ant". % fill move

move_ladybug(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ladybug",
    is_adjacent(position(Q, R, S), position(first_piece_Q, first_piece_R, first_piece_S)),
    position_filled(position(first_piece_Q, first_piece_R, first_piece_S)),
    is_adjacent(position(first_piece_Q, first_piece_R, first_piece_S), position(second_piece_Q, second_piece_R, second_piece_S)),
    position_filled(position(second_piece_Q, second_piece_R, second_piece_S)),
    is_adjacent(position(second_piece_Q, second_piece_R, second_piece_S), position(blank_space_Q, blank_space_R, blank_space_S)),
    \+ position_filled(position(blank_space_Q, blank_space_R, blank_space_S)),
    add_piece(piece(Type, Color, Piled, Next_Q, Next_R, Next_S)). % fill move

move_mosquito(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "mosquito",
    (
        (
            (
                position_filled(position(Q, R, S)),
                move_beetle(piece("beetle", Color, "false", Q, R, S), position(Next_Q, Next_R, Next_S))
            );
            (
                \+ position_filled(position(Q, R, S)),  
                is_adjacent(position(Q, R, S), position(adjacent_piece_Q, adjacent_piece_R, adjacent_piece_S)),
                findall_pieces(piece(Adjacent_Type, _, "false", adjacent_piece_Q, adjacent_piece_R, adjacent_piece_S), [adjacent_piece|_]),
                \+ Adjacent_Type = "mosquito",
                (
                    (
                        assert(piece(Adjacent_Type, Color, "false", Q, R, S)),
                        move_piece(position(Q, R, S), position(Next_Q, Next_R, Next_S))
                    );
                    \+ retract(piece(Adjacent_Type, Color, "false", Q, R, S))    
                )
            )
        ),
        retract(piece(_, _, "false", Next_Q, Next_R, Next_S)),
        assert(piece("mosquito", Color, "false", Next_Q, Next_R, Next_S))   
    ).

move_pillbug(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "pillbug",
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    \+ position_filled(position(Next_Q, Next_R, Next_S)), % no piece in the position
    add_piece(piece(Type, Color, Piled, Next_Q, Next_R, Next_S)). % fill move


move_piece(position(Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    remove_piece(piece(Type, Color, Piled, Q, R, S)), !,
    (
        (
            \+ hive_is_divided,
            Piled = "false"
        );
        \+ add_piece(piece(Type, Color, Piled, Q, R, S))
    ),
    (
        move_beetle(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_queen(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_grasshopper(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_spider(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_ant(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_ladybug(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_mosquito(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        move_pillbug(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S));
        \+ add_piece(piece(Type, Color, Piled, Q, R, S))
    ).