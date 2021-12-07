:- module(actions, [
    op(700, fx, exit), exit/1,
    op(700, fx, move), move/1,
    op(700, fx, initiate), initiate/1,
    op(700, fx, instructions), instructions/1
    ]).

:- [printer].
:- [moves].

% exit.
exit [Exit|_] :-
    Exit = "exit",
    print_exit.

% move <Dim1> <Dim2> <Dim3> to <Next_Dim1> <Next_Dim2> <Next_Dim3>
move [Move, Dim1, Dim2, Dim3, To, Next_Dim1, Next_Dim2, Next_Dim3] :-
    Move = "move",
    atom_number(Dim1, X1),
    atom_number(Dim2, X2),
    atom_number(Dim3, X3),
    To = "to",
    atom_number(Next_Dim1, Y1),
    atom_number(Next_Dim2, Y2),
    atom_number(Next_Dim3, Y3),
    print_move([position(X1, X2, X3), position(Y1, Y2, Y3)]),
    move_piece(position(X1, X2, X3), position(Y1, Y2, Y3)). 

% initiate <Type> <Dim1> <Dim2> <Dim3>
initiate [Init, Type, Dim1, Dim2, Dim3] :-
    Init = "init",
    atom_number(Dim1, X1),
    atom_number(Dim2, X2),
    atom_number(Dim3, X3),
    print_initiate([Type, position(X1, X2, X3)]),
    initiate_piece(piece(Type, "white", _, _, _, _, _), position(X1, X2, X3)). 

% instructions
instructions [Instructions|_] :-
    Instructions = "instructions",
    print_game_instructions.