:- module(printer, [
    print_piece/1,
    print_board/1, 
    print_move/1,
    print_initiate/1,
    print_exit/0,
    print_game_instructions/0
    ]).

:- op(700, yfx, is_represented_by).

% Type is represented by letter
'Queen' is_represented_by 'Q'.
'Beetle' is_represented_by 'B'.
'Grasshopper' is_represented_by 'G'.
'Spider' is_represented_by 'S'.
'Ant' is_represented_by 'A'.
'Ladybug' is_represented_by 'L'.
'Mosquito' is_represented_by 'M'.
'Pillbug' is_represented_by 'P'.

% Color is represented by letter
'Black' is_represented_by 'B'.
'White' is_represented_by 'W'.

print_piece(piece(Type, Color, Piled, Piled_Number, Xpos, Ypos)) :- 
    Type is_represented_by TypeLetter,
    Color is_represented_by ColorLetter,
    write("["),
    write(ColorLetter),
    write(" "),
    write(TypeLetter),
    write(" "),
    write(Xpos),
    write(" "),
    write(Ypos),
    write("]"),
    write("\n").

print_exit :-
    write("Exiting game..."),
    write("\n").

print_move([position(X1, X2, X3), position(Y1, Y2, Y3)]) :-
    write("Move piece from position ["),
    write(X1),
    write(","),
    write(X2),
    write(","),
    write(X3),
    write("] to position ["),
    write(Y1),
    write(","),
    write(Y2),
    write(","),
    write(Y3),
    write("]"),
    write("\n").

print_initiate([Type, position(X1, X2, X3)]) :-
    write("Initiate "),
    write(Type),
    write(" at position ["),
    write(X1),
    write(","),
    write(X2),
    write(","),
    write(X3),
    write("]"),
    write("\n").

print_game_instructions :-
    write("\n"),
    write(" HIVE GAME INSTRUCTIONS \n"),
    write("  ------------------------------------------------------------------------------------------------------------------------- \n"),
    write("  |[ Queen Moves      ] - A Queen can move                  |\n"),
    write("  |[ Beetle Moves     ] - A Beetle can move                 |\n"),
    write("  |[ Grasshopper Moves] -                                   |\n"),
    write("  |[ Spider Moves     ] -                                   |\n"),
    write("  |[ Ant Moves        ] -                                   |\n"),
    write("  |[ Ladybug Moves    ] -                                   |\n"), 
    write("  |THE RESTRICTION FOR ALL MOVES: Move is only legal if friendly piece is not in it's way                                  |\n"),
    write("  |________________________________________________________________________________________________________________________|\n"),
    write("\n"),
    write(" ENJOY THE GAME (^.^)o[~] !"),
    write("\n").

print_board([]).
print_board([X|Y]) :-
    print_piece(X),
    print_board(Y).
