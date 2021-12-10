get_pos_text(Q, R, Text) :-
    (
        Q > 0,
        atom_concat('+', Q, Row)
    );
    Row is Q,
    write(Row),
    write('\n'),
    (
        R > 0,
        atom_concat('+', R, Col)
    );
    Col is R,
    write(Col),
    write('\n'),

    atom_concat(Row, Col, Text).

:- get_pos_text(1, -2, Text),
    write(Text).