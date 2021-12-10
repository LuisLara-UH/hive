:- module(printer, [
    print_piece/1,
    print_board/1, 
    print_move/1,
    print_initiate/1,
    print_exit/0,
    print_game_instructions/0,
    print_game_state/0,
    print_positions/1,
    print_game_options/0
    ]).

:- op(700, yfx, is_represented_by).

:- [piece].

% Type is represented by letter
"queen" is_represented_by "Q".
"beetle" is_represented_by "B".
"grasshopper" is_represented_by "G".
"spider" is_represented_by "S".
"ant" is_represented_by "A".
"ladybug" is_represented_by "L".
"mosquito" is_represented_by "M".
"pillbug" is_represented_by "P".

% Color is represented by letter
"black" is_represented_by "B".
"white" is_represented_by "W".

print_piece(piece(Type, Color, Piled, Piled_Number, Q, R, S)) :- 
    Type is_represented_by TypeLetter,
    Color is_represented_by ColorLetter,
    write("["),
    write(ColorLetter),
    write(" "),
    write(TypeLetter),
    write(" "),
    write(Q),
    write(" "),
    write(R),
    write(" "),
    write(S),
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
    write("]..."),
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
    write("]..."),
    write("\n").

print_game_instructions :-
    write("\n"),
    write("  HIVE RULES:\n"),
    write("   ------------------------------------------------------------------------------------------------------------------------ \n"),
    write("  |The goal of the game is to totally surround the Queen of the opponent.                                                  |\n"),
    write("  |For each turn you can decide either to move a piece or to place a new one. A new piece may not be placed next to        |\n"),
    write("  |oppenent's pieces                                                                                                       |\n"),
    write("  |[ Queen       ] - Moves one space per turn                                                                              |\n"),
    write("  |[ Beetle      ] - Moves one space per turn and can move on top of the hive                                              |\n"),
    write("  |[ Grasshopper ] - Jumps rom its space over any number of pieces (at least one) to the next unoccupied space along a     |\n"),
    write("  |                  straight row of joined pieces                                                                         |\n"),
    write("  |[ Spider      ] - Moves exactly 3 spaces per turn without backtrack                                                     |\n"),
    write("  |[ Ant         ] - Moves anywhere around the Hive                                                                        |\n"),
    write("  |[ Ladybug     ] - Moves three spaces: two ontop of the Hive, then one down                                              |\n"), 
    write("  |[ Mosquito    ] - Takes on the movement ability of any creature of either color that it's touching                      |\n"), 
    write("  |[ Pillbug     ] - Moves ne space at a time and allows to move an adjacent un-stacked piece two spaces                   |\n"), 
    write("  |THE RESTRICTION FOR ALL MOVES:                                                                                          |\n"),
    write("  |- One Hive rule                                                                                                         |\n"),
    write("  |________________________________________________________________________________________________________________________|\n"),
    write("\n"),
    write("  ENJOY THE GAME (^.^)o[~] !"),
    write("\n").

print_game_options :-
    write("  CHOOSE A GAME MODE:\n"),
    write("  1. WHITE PLAYER vs BLACK MACHINE\n"),
    write("  2. BLACK PLAYER vs WHITE MACHINE\n"),
    write("  3. PLAYER vs PLAYER\n"),
    write("  4. MACHINE vs MACHINE\n"),
    write('Example: \"1\".\n' ).

print_winner(Color) :-
    write(Color),
    write(" WINS!!!\n").

print_board([]).
print_board([X|Y]) :-
    print_piece(X),
    print_board(Y).

print_game_state :-
    write("Pieces:\n"),
    get_pieces(Pieces),
    print_board(Pieces).

print_positions([]).
print_positions([Pos|Other_Pos]) :-
    write(Pos),
    write("\n"),
    print_positions(Other_Pos).
