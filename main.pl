:- [actions].
:- [piece].
:- [printer].

:- op(700, fx, execute_action).

execute_action Action :- 
    (
        (
            move Action;
            initiate Action;
            instructions Action;
            \+ write("Action not permited.\n")
        ),
        fail
    );
    exit Action.

execute_game :-
    repeat,
    print_game_state,
    read(Input),
    atom_string(Input, Processed),
    string_lower(Processed, Action),
    split_string(Action, " ", " ", L),
    execute_action L. 

start_game :-
    print_game_instructions,
    add_piece(piece("queen", "white", "false", 0, 0, 0, 0)),
    add_piece(piece("queen", "black", "false", 0, 0, -1, 1)),
    execute_game.