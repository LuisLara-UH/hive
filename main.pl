:- [actions].
:- [piece].
:- [printer].
:- [player].
:- [turn].
:- [ia_color].

:- op(700, fx, execute_action).

execute_action Action :- 
    (
        move Action;
        initiate Action;
        instructions Action
    ),
    exit Action.

execute_player_action :-
    turn_color(Color),
    write(Color),
    write(" player's turn.\n"),
    read(Input),
    atom_string(Input, Processed),
    string_lower(Processed, Action),
    split_string(Action, " ", " ", L),
    execute_action L, !. 

execute_ai_action :-
    turn_color(Color),
    write(Color),
    write(" AI turn.\n"),
    get_pieces(Pos),
    Depth = 1,
    alphabeta(Pos, 0, 40, BestSucc, Val, Depth, Color),
    %minimax(Pos, BestSucc, Val, Depth, Color),
    write("Best move found: "),
    write(BestSucc),
    write("\n"),
    restart_game(BestSucc),
    change_turn,
    !, fail. 

execute_game :-
    repeat,
    print_game_state,
    turn_color(Color),
    (
        (ai(Color), execute_ai_action);
        execute_player_action
    ). 

start_game :-
    print_game_instructions,
    add_ai("black"),
    add_piece(piece("queen", "white", "false", 0, 0, 0, 0)),
    add_piece(piece("queen", "black", "false", 0, 0, -1, 1)),
    execute_game.