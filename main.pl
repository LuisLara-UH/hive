:- [actions].
:- [printer].

:- op(700, fx, execute_action).

execute_action Action :- 
    (
        (
            move Action;
            initiate Action;
            instructions Action
        ),
        fail
    );
    exit Action.

execute_game :-
    repeat,
    read(Input),
    atom_string(Input, Processed),
    string_lower(Processed, Action),
    split_string(Action, " ", " ", L),
    execute_action L. 

start_game :-
    print_game_instructions,
    execute_game.