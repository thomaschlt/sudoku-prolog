%% * --------------------------------------------------------
%% * Sudoku Solver (sudoku_solver.pl)
%% *
%% * Description : Prolog program for solving a Sudoku grid using Constraint Logic Programming over Finite Domains (CLPFD) 
%% * Author: Thomas Chimbault
%% * February ~ March 2024  
%% * --------------------------------------------------------

:- use_module('sudoku_utils.pl').

%% * --------------------------------------------------------
%% * Main predicate to solve the Sudoku
%% * --------------------------------------------------------

solver(InputFile, OutputFile) :-
	extract_data(Entry, InputFile), !,	
	length(Entry, PuzzleSize),
	sudoku(Entry, Solution),
	write_sequence_in_grid(OutputFile, Solution).
