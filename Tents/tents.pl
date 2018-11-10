% bb_min and ic libraries are used in this program
:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This predicate acts like a "main" predicate
tents(RowTents, ColumnTents, Trees, Tents):-
	length(RowTents,N),
	length(ColumnTents,M),
	
	% Create a list with N elements
	length(Solution,N),
	
	% Make each of these element a list with M length and define its domain 
	initialize(Solution,M),
	
	% Constrain Predicate
	constrain(RowTents,ColumnTents,Trees,Solution,N,M),
	
	% Flattening our solution
	flatten(Solution,FlattenedSolution),
	
	% Cost Function
	MinCost #= sum(FlattenedSolution),
	
	% Find Solution with minimum cost
	bb_min(search(FlattenedSolution, 0, first_fail, indomain_middle, complete, []),MinCost, bb_options{strategy:restart}),
	
	
	% Create a list with N elements
	length(NewSol,N),
	
	% Make each of these element a list with M length and define its domain 
	initialize(NewSol,M),
	
	% Constrain Predicate
	constrain(RowTents,ColumnTents,Trees,NewSol,N,M),
	
	% Flattening our solution
	flatten(NewSol,NewFlat),
	
	% Make sure this random Solution has the minimum cost found by bb_min
	sum(NewFlat) #= MinCost,
	
	% Find a random Solution
	search(NewFlat, 0, first_fail, indomain,complete, []),
	
	% Create Final Result
	make_coordinates(NewFlat,Tents,N,M).
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize([],_).
initialize([S|Ss],M):-
	length(S,M),
	S #:: 0..1,
	initialize(Ss,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain(RowTents,ColumnTents,Trees,Solution,N,M):-
	not_on_tree(Trees,Solution),
	row_limit(RowTents,Solution),
	column_limit(ColumnTents,Solution),
	at_least_one_tent(Solution,Trees,N,M),
	not_adjacent(Solution,N,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_on_tree(Trees,Solution):-
	not_on_tree(Trees,Solution,1,1).
	
not_on_tree(_,[],_,_).
	
not_on_tree(Trees,[[]|RestRows],I,J):-
	NewI is I + 1,
	not_on_tree(Trees,RestRows,NewI,J).

	
not_on_tree([X-Y|Ts],[[C|Cs]|RestRows],X,Y):-!,
	C #= 0,
	NewJ is Y + 1,
	not_on_tree(Ts,[Cs|RestRows],X,NewJ).
	
not_on_tree(Trees,[[_|Cs]|RestRows],I,J):-
	NewJ is J + 1,
	not_on_tree(Trees,[Cs|RestRows],I,NewJ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

row_limit([],[]).
row_limit([-1|Ls],[_|Rs]):-
	row_limit(Ls,Rs).
row_limit([Limit|Ls],[Row|Rs]):-
	Limit #>= sum(Row), 
	row_limit(Ls,Rs).
	
column_limit(ColumnTents,Solution):-
	transpose(Solution,Trans),
	row_limit(ColumnTents,Trans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

at_least_one_tent(_,[],_,_).
at_least_one_tent(Solution,[T|Ts],N,M):-
	find_adjacents(Solution,T,N,M,Adj),
	sum(Adj)  #>= 1 ,
	at_least_one_tent(Solution,Ts,N,M).

find_adjacents(Solution,T,N,M,[A1,A2,A3,A4,A5,A6,A7,A8]):-
	add_adjacent1(Solution,T,N,M,A1),
	add_adjacent2(Solution,T,N,M,A2),
	add_adjacent3(Solution,T,N,M,A3),
	add_adjacent4(Solution,T,N,M,A4),
	add_adjacent5(Solution,T,N,M,A5),
	add_adjacent6(Solution,T,N,M,A6),
	add_adjacent7(Solution,T,N,M,A7),
	add_adjacent8(Solution,T,N,M,A8).

% Each cell of the matrix has at most 8 adjacents
	
add_adjacent1(Solution,X-Y,_,_,Adj):-
	Y-1 > 0,
	!,
	NewY is Y-1,
	add(Solution,X,NewY,Adj).
add_adjacent1(_,_,_,_,0).	

	
add_adjacent2(Solution,X-Y,_,M,Adj):-
	Y+1 =< M ,
	!,
	NewY is Y+1,
	add(Solution,X,NewY,Adj).
add_adjacent2(_,_,_,_,0).


add_adjacent3(Solution,X-Y,_,_,Adj):-
	X-1 > 0,
	!,
	NewX is X -1,
	add(Solution,NewX,Y,Adj).
add_adjacent3(_,_,_,_,0).

add_adjacent4(Solution,X-Y,N,_,Adj):-
	X+1 =< N,
	!,
	NewX is X+1,
	add(Solution,NewX,Y,Adj).
add_adjacent4(_,_,_,_,0).

add_adjacent5(Solution,X-Y,_,_,Adj):-
	X-1 >0,
	Y-1 >0,
	!,
	NewX is X-1,
	NewY is Y-1,
	add(Solution,NewX,NewY,Adj).
add_adjacent5(_,_,_,_,0).

add_adjacent6(Solution,X-Y,_,M,Adj):-
	X-1 >0,
	Y+1 =< M,
	!,
	NewX is X-1,
	NewY is Y+1,
	add(Solution,NewX,NewY,Adj).
add_adjacent6(_,_,_,_,0).


add_adjacent7(Solution,X-Y,N,_,Adj):-
	X+1 =< N,
	Y-1 > 0,
	!,
	NewX is X+1,
	NewY is Y-1,
	add(Solution,NewX,NewY,Adj).
add_adjacent7(_,_,_,_,0).

add_adjacent8(Solution,X-Y,N,M,Adj):-
	X+1 =< N,
	Y+1 =< M,
	!,
	NewX is X+1,
	NewY is Y+1,
	add(Solution,NewX,NewY,Adj).
add_adjacent8(_,_,_,_,0).

add(Solution,X,Y,Adj):-
	add(Solution,Adj,X,Y,1,1).

	
add([[Adj|_]|_],Adj,X,Y,X,Y):-!.
	
add([[]|RestRows],Adj,X,Y,I,_):-
	NewI is I + 1,
	add(RestRows,Adj,X,Y,NewI,1).
add([[_|RestColumns]|RestRows],Adj,X,Y,I,J):-
	NewJ is J + 1,
	add([RestColumns|RestRows],Adj,X,Y,I,NewJ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_adjacent(Solution,N,M):-
	not_adjacent(Solution,N,M,1,1,Solution).
	
not_adjacent([],_,_,_,_,_).

not_adjacent([[]|Rs],N,M,I,_,S):-
	NewI is I + 1,
	not_adjacent(Rs,N,M,NewI,1,S).
	
not_adjacent([[C|Cs]|Rs],N,M,I,J,Solution):-
	find_adjacents(Solution,I-J,N,M,Adjacents),
	check_each_adjacent(Adjacents,C),
	NewJ is J + 1,
	not_adjacent([Cs|Rs],N,M,I,NewJ,Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_each_adjacent([],_).
check_each_adjacent([A|As],C):-
	A + C #=< 1,
	check_each_adjacent(As,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_coordinates(Solution,Tents,N,M):-
	make_coordinates(Solution,Tents,N,M,1,1).
	
make_coordinates([],[],_,_,_,_).	
	
make_coordinates([1|Ss],[I-J|Tents],N,M,I,J):-
	0 is J mod M,!,
	NewI is I + 1,
	make_coordinates(Ss,Tents,N,M,NewI,1).
make_coordinates([1|Ss],[I-J|Tents],N,M,I,J):-
	NewJ is J + 1,
	make_coordinates(Ss,Tents,N,M,I,NewJ).


make_coordinates([0|Ss],Tents,N,M,I,J):-
	0 is J mod M,!,
	NewI is I + 1,
	make_coordinates(Ss,Tents,N,M,NewI,1).
make_coordinates([0|Ss],Tents,N,M,I,J):-
	NewJ is J + 1,
	make_coordinates(Ss,Tents,N,M,I,NewJ).

%%%%%%%%%%%%%%%%%%%%%%%%  transpose/2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transpose([], []).
transpose([F|Fs], Trans):-
    transpose(F,[F|Fs], Trans).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]):-
	place_first(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).

place_first([], [], []).
place_first([[F|Os]|Rest], [F|Fs], [Os|Oss]):-
	place_first(Rest, Fs, Oss).

%%%%%%%%%%%%%%%%%%%%%%%%  flatten1/2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flatten1([],[]):-!.
flatten1([H|T],F):-!,
	flatten1(H,FH),
	flatten1(T,FT),
	append(FH,FT,F).
flatten1(X,[X]).

