%% * --------------------------------------------------------
%% * Sudoku Solver
%% *
%% * Description : Prolog program for solving a Sudoku grid using Constraint Logic Programming over Finite Domains (CLPFD) 
%% * Author: Thomas Chimbault
%% * February ~ March 2024  
%% * --------------------------------------------------------

:- use_module(library(pio)).
:- use_module(library(clpfd)).

%% * --------------------------------------------------------
%% * Main predicate to solve the Sudoku
%% * --------------------------------------------------------

solver(InputFile, OutputFile) :-
	extract_data(Entry, InputFile), !,	
	length(Entry, PuzzleSize),
	sudoku(Entry, Solution),
	write_sequence_in_grid(OutputFile, Solution).


%% * --------------------------------------------------------
%% * Extract sudoku data
%% * --------------------------------------------------------

extract_data(Entry, PathToFile) :-
	phrase_from_file(sudoku_input(Entry), PathToFile).

sudoku_input([]) --> [].
sudoku_input([_|Entry]) --> whitespace, "X", whitespace, sudoku_input(Entry).
sudoku_input([N|Entry]) --> whitespace, nat(N), whitespace, sudoku_input(Entry).
sudoku_input(Entry) --> whitespace, "|", whitespace, sudoku_input(Entry).
sudoku_input(Entry) --> whitespace, "-", whitespace, sudoku_input(Entry).
% sudoku_input(Entry) --> whitespace, "+", whitespace, sudoku_input(Entry).
sudoku_input(Entry) --> whitespace, "\n", whitespace, sudoku_input(Entry).

% whitespace 
whitespace  --> " "; "+"; "-"; ""; "  "; "   ".

% DCG rules for numbers
char_int(0) --> "0". 
char_int(1) --> "1".
char_int(2) --> "2".
char_int(3) --> "3".
char_int(4) --> "4".
char_int(5) --> "5".
char_int(6) --> "6".
char_int(7) --> "7".
char_int(8) --> "8".
char_int(9) --> "9".

nat(N)   --> char_int(D), nat(D,N).
nat(A,N) --> char_int(D), { A1 is A*10 + D }, nat(A1,N).
nat(N,N) --> [].


%% * --------------------------------------------------------
%% * Write the solution in a grid
%% * --------------------------------------------------------

write_sequence_in_grid(Filename, List) :-
	open(Filename, write, File),
	length(List, PuzzleSize),
	Size is floor(sqrt(sqrt(PuzzleSize))),
	write_in_grid(File, List, Size, 1, 1),
	close(File).

write_in_grid(_File, [], _, _, _) :- !.

% ! handle the base case of the recursion
write_in_grid(File, List, Size, X, Y) :-
	Y mod (Size+1) =:= 0, % * check if Y is at the end of a row
	Y1 is Y + 1, % * next row
	NumOfElements is Size * Size,
	add_separator_line(File, Size, NumOfElements, 1),
	write(File, '\n'),
	write_in_grid(File, List, Size, X, Y1), !.

% ! handle the recursive case where the list is not empty
write_in_grid(File, [H|T], Size, X, Y) :-
	NeedABar  is X mod Size,
	EndOfLine is X mod (Size * Size),
	X1 is X + 1,
	Y1 is Y + 1,
	write(File, ' '), write(File, H), write(File, ' '),
	%% If Pos mod SquareSize = 0, then print '|'
	(NeedABar =:= 0, EndOfLine > 0
		-> write(File, '|')
		;  true),
	%% If X mod (SquareSize * SquareSize) = 0, then print '\n'
	(EndOfLine =:= 0 -> write(File, '\n'), write_in_grid(File, T, Size, X1, Y1) ;  write_in_grid(File, T, Size, X1, Y)).

add_separator_line(_, Size, Pos, Pos1) :-
	Pos1 is Pos + Size.

add_separator_line(File, Size, NumOfElements, Pos) :-
	Pos1 is Pos + 1,
	(Pos mod (Size+1) =:= 0 -> write(File, '-') ;  write(File, '---')),
	add_separator_line(File, Size, NumOfElements, Pos1).

%% * -----------------------------------------------------------------------------
%% * Solving grid using CLPFD (Constraint Logic Programming over Finite Domains)
%% * -----------------------------------------------------------------------------

sudoku(Puzzle, Solution) :-
	length(Puzzle, PuzzleSize),
	Size is floor(sqrt(PuzzleSize)),
	Solution = Puzzle,
	apply_constraints(Solution, Size),
	labeling([ff,enum],Puzzle).


% ! Apply constraints to Sudoku solution
apply_constraints(Solution, Size) :-
    Solution ins 1..Size,
    split_list(Size, Solution, Rows),
    maplist(all_different, Rows),
    transpose(Rows, Columns),
    maplist(all_different, Columns),
    create_squares(Solution, Squares, Size, 0),
    maplist(all_different, Squares).

% ! Split a list into sublists of a given length
split_list(Length,List,ListOfLists) :-
	split_list(Length,Length,List,ListOfLists).

split_list(Length,Offset,List,Lsg) :-
	split_list_aux(Length,Offset,nil,List,[],Lsg).

split_list_aux(Length,_,nil,List,Acc,RAcc) :-
	length(List,PuzzleSize), PuzzleSize < Length, !,
	reverse(Acc,RAcc).

split_list_aux(Length,Offset,Buffer,List,Acc,Lsg) :-
	length(Temp,Length),
	append(Temp,_Rest,List),
	length(Temp2,Offset),
	append(Temp2,Rest2,List),
	split_list_aux(Length,Offset,Buffer,Rest2,[Temp|Acc],Lsg).

% ! Create a list of sublists representing the squares of the Sudoku
create_squares(_, Square, Size, I) :- 
	I is Size * Size, 
	length(Square, Size), 
	sublist_length(Square, Size).

create_squares([H|T], Square, Size, I) :-
	square_pointer(Size, X, Y, I), 
	combine_to_squares(H, Square, X, Y),
	I1 is I + 1,
	create_squares(T, Square, Size, I1).

sublist_length([], _).
sublist_length([H|T], Length) :- 
	length(H, Length),
	sublist_length(T, Length).

combine_to_squares(Item, Values, X, Y) :-
	nth0(X, Values, Bucket),
	nth0(Y, Bucket, Item).

% ! Get the X and Y coordinates of a given index
square_pointer(Size, X, Y, I) :- 
	Size_Sqrt is floor(sqrt(Size)),
	X is (I mod Size // Size_Sqrt) + (Size_Sqrt * (I // (Size * Size_Sqrt))),
	Y is (I mod Size_Sqrt) + (Size_Sqrt * ((I mod (Size * Size_Sqrt)) // Size)).