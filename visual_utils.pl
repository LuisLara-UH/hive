% get_min_col(Min_Col) :- !.

% get_min_row() :- !.

% get_max_col() :- !.

% get_max_row() :- !.

concat_to_string([X | Y], Text) :-
    append([X], Y, Text).

% min_col_is_even(X) :-
%     0 = X // 2.

get_number_text(X, Y, Text) :-
    (
        X > 0,
        concat_to_string(['+', X], Row)
    );
    concat_to_string(['', X], Row),
    write('row: '),
    write(Row),
    write('\n'),
    (
        Y > 0,
        concat_to_string(['+', Y], Col)
    );
    concat_to_string(['', Y], Col),
    write('col: '),
    write(Col),
    write('\n'),
    concat_to_string([Col, ',', Row], Text).

print_list([]).
print_list([elem|rest]) :-
    write('nadia'),
    write(elem),
    print_list(rest).

first_even_row_list([box(_, Q, R) | Row], Matrix_Row) :-
    get_number_text(Q, R, RowCol),
    Empty_Box_Plus_Postion = ['      ', Q, ',' , R],
    write(RowCol),
    % print_list(RowCol),
    concat_to_string(Empty_Box_Plus_Postion, New_Boxes),
    append(New_Boxes, Matrix_Row, New_Matrix_Row),
    first_even_row_list(Row, New_Matrix_Row).

% first_odd_row_list([box(_, Q, R) | Row], Matrix_Row) :-
%     get_number_text(Q, String_Row),
%     get_number_text(R, String_Col),
%     Position_Plus_Empty_Box = [String_Row, ',', String_Col, '      '],
%     concat_to_string(Position_Plus_Empty_Box, New_Boxes),
%     append(New_Boxes, Matrix_Row, New_Matrix_Row),
%     first_odd_row_list(Row, New_Matrix_Row).


% make_matrix() :- 
%     get_min_col(MinCol),
%     get_min_row(MinRow),
%     get_max_col(MaxCol),
%     get_max_row(MaxRow),

%     (
%         min_col_is_even(),

%     );
%     (

%     ).
    
    
:- X = [box(_, 0, -2)],
first_even_row_list(X, Matrix_Row),
write(Matrix_Row).