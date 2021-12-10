:- [actions].
:- [piece].
:- [printer].
:- [player].
:- [turn].
:- [ia_color].
:- [utils].

:- op(700, fx, execute_action).

game_mode(Option) :-
    Option = "1",
    add_ai("black").

game_mode(Option) :-
    Option = "2",
    add_ai("white").

game_mode(Option) :-
    Option = "3".

game_mode(Option) :-
    Option = "4",
    add_ai("black"),
    add_ai("white").

check_lose(Color) :- 
    get_pieces(Pieces),
    find_next_positions(Pieces, [], Positions),
    length(Positions, Amount),
    Amount = 0,
    enemy_color(Color, Win_Color).

execute_action Action :- 
    (
        move Action;
        initiate Action;
        instructions Action
    ),
    exit Action.

execute_player_action :-
    turn_color(Color),
    \+ ai(Color),
    write(Color),
    write(" player's turn.\n"),
    read(Input),
    atom_string(Input, Processed),
    string_lower(Processed, Action),
    split_string(Action, " ", " ", L), !,
    execute_action L. 

execute_ai_action :-
    turn_color(Color),
    ai(Color),
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
    check_lose(Color);
    (execute_player_action; execute_ai_action). 

start_game :-
    print_game_instructions,
    repeat,
    print_game_options,
    read(Option),
    game_mode(Option),
    add_piece(piece("queen", "white", "false", 0, 0, 0, 0)),
    add_piece(piece("queen", "black", "false", 0, 0, -1, 1)),
    execute_game.