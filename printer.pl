:- module(printer, [print_piece/1, print_board/1]).

:- op(700, yfx, is_represented_by).
:- op(700, fx, print_piece).
:- op(700, fx, print_board).

% Type is represented by letter
Queen is_represented_by 'Q'.
Beetle is_represented_by 'B'.
Grasshopper is_represented_by 'G'.
Spider is_represented_by 'S'.
Ant is_represented_by 'A'.
Ladybug is_represented_by 'L'.
Mosquito is_represented_by 'M'.
Pillbug is_represented_by 'P'.

% Color is represented by letter
Black is_represented_by 'B'.
White is_represented_by 'W'.

print_piece piece(Type, Color, Xpos, Ypos) :- 
    Type is_represented_by TypeLetter,
    Color is_represented_by ColorLetter,
    write("["),
    write(ColorLetter),
    write(" "),
    write(TypeLetter),
    write(" "),
    write(Xpos),
    write(Ypos),
    write("]"),
    write("\n").

print_board [X|Y] :-
    print_piece X,
    print_board Y.