:- module(moves, [
    op(700, fx, initiate_piece), initiate_piece/1, 
    op(700, fx, move_piece), move_piece/1, 
    op(250, yfx, to), to/2, 
    op(250, yfx, in), in/2]).

% imports
:- [piece].

% operators
:- op(700, fx, move_queen).
:- op(700, fx, move_beetle).
:- op(700, fx, move_grasshopper).
:- op(700, fx, move_spider).
:- op(700, fx, move_ant).
:- op(700, fx, move_ladybug).
:- op(700, fx, move_mosquito).
:- op(700, fx, move_pillbug).

% piece(type, black or white, Piled, Q, R, S)

% initiate piece
initiate_piece piece(Type, Color, _,  _, _, _) in position(Q, R, S) :- !. % fill initiate

% move piece
move_queen piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S) :- 
    !. % fill move

move_beetle piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move

move_grasshopper piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move

move_spider piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move

move_ant piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move

move_ladybug piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move

move_mosquito piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move

move_pillbug piece(_, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    !. % fill move


move_piece piece(Beetle, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_beetle piece(Beetle, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Queen, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_queen piece(Queen, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Grasshopper, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_grasshopper piece(Grasshopper, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Spider, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_spider piece(Spider, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Ant, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_ant piece(Ant, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Ladybug, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_ladybug piece(Ladybug, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Mosquito, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_mosquito piece(Mosquito, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).

move_piece piece(Pillbug, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S)  :- 
    move_pillbug piece(Pillbug, Color, Piled,  Q, R, S) to position(Next_Q, Next_ R, Next_S).