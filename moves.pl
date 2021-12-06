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
    \+ findall_pieces(piece(_, _, _, Next_Q, Next_R, Next_S), [_|_]), % no piece in the position
    add_piece(piece(Type, Color, Piled, Next_Q, Next_R, Next_S)).

move_beetle(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "beetle",
    is_adjacent(position(Q, R, S), position(Next_Q, Next_R, Next_S)),
    (
        \+ findall_pieces(piece(_, _, _, Next_Q, Next_R, Next_S), [_|_]); % no piece in the position
        (   % Set unpiled piece as piled
            remove_piece(piece(Type, Color, "false", Next_Q, Next_R, Next_S),
            add_piece(piece(Type, Color, "true", Next_Q, Next_R, Next_S)
        )
    ),
    add_piece(piece(Type, Color, Piled, Next_Q, Next_R, Next_S)). % fill move

move_grasshopper(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "grasshopper". % fill move

move_spider(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "spider". % fill move

move_ant(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ant". % fill move

move_ladybug(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "ladybug". % fill move

move_mosquito(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "mosquito". % fill move

move_pillbug(piece(Type, Color, Piled,  Q, R, S), position(Next_Q, Next_R, Next_S))  :- 
    Type = "pillbug". % fill move


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