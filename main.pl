:- [actions].
:- [piece].
:- [printer].
:- [player].
:- [turn].
:- [ia_color].
:- [utils].
:- [moves_utils].

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
    length(Pieces, Pieces_Amount),
    Pieces_Amount > 0,
    find_next_positions(Pieces, [], Positions),
    restart_game(Pieces),
    length(Positions, Amount), !,
    Amount is 0,
    enemy_color(Color, Win_Color),
    print_winner(Win_Color).

check_lose(Color) :-
    findall_pieces(piece("queen", Color, _,_,_,_,_), [piece(_,_,_,_,Q,R,S)|_]),
    is_adjacent(position(Q, R, S), position(Adj_Q, Adj_R, Adj_S)),
    findall_pieces(piece(_,_,_,_,Adj_Q, Adj_R, Adj_S), []),
    enemy_color(Color, WinColor),
    print_winner(Win_Color).

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
    write(" AI turn.\nThinking...\n"),
    get_pieces(Pos),
    (
        (
          length(Pos, Pieces_Amount),
          Pieces_Amount = 0,
          BestSucc = [piece("queen", Color, "false", 0, 0, 0, 0)]
        );
        (
            Depth = 1, % change alphabeta depth
            alphabeta(Pos, 20, 30, BestSucc, Val, Depth, Color)
            %minimax(Pos, BestSucc, Val, Depth, Color)
        )
    ), 
    restart_game(BestSucc),
    ((\+ turn_color(Color)); change_turn),
    write(Color),
    write("\n"),
    turn_color(TurnColor),
    write(TurnColor),
    write("\n"),
    !, fail. 

execute_game :-
    repeat,
    print_game_state,
    turn_color(Color),
    (
        check_lose(Color);
        (execute_player_action; execute_ai_action)
    ). 

start_game :-
    print_game_instructions,
    repeat,
    print_game_options,
    read(Option),
    game_mode(Option),
    %add_piece(piece("queen", "white", "false", 0, 0, 0, 0)),
    %add_piece(piece("queen", "black", "false", 0, 0, -1, 1)),
    execute_game.