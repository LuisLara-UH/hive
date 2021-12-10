piece_drawing(pos(Q, R), Type, Text) :-
    Text = format("  ~q,~r  \n /     \ \n/       \\n\       /\n \_____/", [Q, R, Type]).
    

piece_drawing(pos(-1, -1), "A", Text),
write(Text).



% file_search_path(library,pce('prolog/lib')).

% :- use_module(library(pce)).

% hello:- new(D, dialog('Demo Windows')), send(d, open).


% new(P, point(10, 20)).
% new(@demo, dialog('Demo Window')).
% send(P, x(15)).
% send(@demo, append(text_item(name))).
% send(@demo, open).
% :- 
%     new(P, picture('Hello World')),
%     send(P, display, text('Hello World'), point(20, 20)),
%     send(P, open).