:- op(700, fx, move_piece).
:- op(700, fx, move_queen).
:- op(700, fx, move_beetle).
:- op(700, fx, move_grasshopper).
:- op(700, fx, move_spider).
:- op(700, fx, move_ant).
:- op(700, fx, move_ladybug).
:- op(700, fx, move_mosquito).
:- op(700, fx, move_pillbug).

move_queen piece(_, Color, Xpos, Ypos) :- !. % fill move

move_beetle piece(_, Color, Xpos, Ypos) :- !. % fill move

move_grasshopper piece(_, Color, Xpos, Ypos) :- !. % fill move

move_spider piece(_, Color, Xpos, Ypos) :- !. % fill move

move_ant piece(_, Color, Xpos, Ypos) :- !. % fill move

move_ladybug piece(_, Color, Xpos, Ypos) :- !. % fill move

move_mosquito piece(_, Color, Xpos, Ypos) :- !. % fill move

move_pillbug piece(_, Color, Xpos, Ypos) :- !. % fill move

move_piece piece("Beetle", Color, Xpos, Ypos) :- move_beetle piece("Beetle", Color, Xpos, Ypos).
move_piece piece("Queen", Color, Xpos, Ypos) :- move_queen piece("Queen", Color, Xpos, Ypos).
move_piece piece("Grasshopper", Color, Xpos, Ypos) :- move_grasshopper piece("Grasshopper", Color, Xpos, Ypos).
move_piece piece("Spider", Color, Xpos, Ypos) :- move_spider piece("Spider", Color, Xpos, Ypos).
move_piece piece("Ant", Color, Xpos, Ypos) :- move_ant piece("Ant", Color, Xpos, Ypos).
move_piece piece("Ladybug", Color, Xpos, Ypos) :- move_ladybug piece("Ladybug", Color, Xpos, Ypos).
move_piece piece("Mosquito", Color, Xpos, Ypos) :- move_mosquito piece("Mosquito", Color, Xpos, Ypos).
move_piece piece("Pillbug", Color, Xpos, Ypos) :- move_pillbug piece("Pillbug", Color, Xpos, Ypos).