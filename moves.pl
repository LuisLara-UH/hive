:- module(moves, [initiate_piece/1, move_piece/1, to/2, in/2]).

% imports
:- [piece].

% operators
:- op(700, fx, initiate_piece).

:- op(700, fx, move_piece).
:- op(700, fx, move_queen).
:- op(700, fx, move_beetle).
:- op(700, fx, move_grasshopper).
:- op(700, fx, move_spider).
:- op(700, fx, move_ant).
:- op(700, fx, move_ladybug).
:- op(700, fx, move_mosquito).
:- op(700, fx, move_pillbug).

:- op(250, yfx, to).
:- op(250, yfx, in).

% piece(type, black or white, Piled, Row, Column)

% initiate piece
initiate_piece piece(Type, Color, _,  _, _) in position(Row, Column) :- !. % fill initiate

% move piece
move_queen piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column) :- 
    !. % fill move

move_beetle piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move

move_grasshopper piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move

move_spider piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move

move_ant piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move

move_ladybug piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move

move_mosquito piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move

move_pillbug piece(_, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    !. % fill move


move_piece piece(Beetle, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_beetle piece(Beetle, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Queen, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_queen piece(Queen, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Grasshopper, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_grasshopper piece(Grasshopper, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Spider, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_spider piece(Spider, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Ant, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_ant piece(Ant, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Ladybug, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_ladybug piece(Ladybug, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Mosquito, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_mosquito piece(Mosquito, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).

move_piece piece(Pillbug, Color, Piled,  Row, Column) to position(Next_Row, Next_Column)  :- 
    move_pillbug piece(Pillbug, Color, Piled,  Row, Column) to position(Next_Row, Next_Column).