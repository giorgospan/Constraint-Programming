% branch_and_bound and ic libraries are used in this program 
% Received "stack overflow" error for N=800 nodes  
:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate acts like a "main" predicate
maxclq(N,D,Clique,Size):-
	create_graph(N,D,Graph),
	find_non_connected(Graph,N,NonConnected),
	length(Solution,N),
	Solution #:: 0..1,
	constrain(Solution,NonConnected),
	cost(Solution,N,Cost),
	bb_min(search(Solution, 0, first_fail, indomain,complete, []),Cost, bb_options{strategy:restart}),
	process_solution(Solution,1,[],Clique,0,Size). 	
	% 0 is our accumulator for finding sum of nodes in solution list
	% [] is our accumulator for finding the nodes that belong to the max clique	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cost(Solution,N,Cost):-
	Cost #= N - Sum,
	find_sum(Solution,0,Sum).  % 0 is our accumulator

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_sum([],FinalSum,FinalSum).
find_sum([S|Ss],Acc,FinalSum):-
	NewAcc #= Acc + S,
	find_sum(Ss,NewAcc,FinalSum).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain(_,[]).

constrain(Solution,[X-Y|Rest]):-
	match(Solution,X,Node1),
	match(Solution,Y,Node2),
	Node1 + Node2 #=< 1,
	constrain(Solution,Rest).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Matches the n-th element of a list to the third argument
match([H|_],1,H) :-
    !.
match([_|T],N,H) :-
    N > 1, %add for loop prevention
    N1 is N-1,
    match(T,N1,H).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% "Creates" Clique and Size
process_solution([],_,Clique,Clique,FinalSum,FinalSum).

process_solution([1|Ss],Index,SoFarClique,FinalClique,SoFarSum,FinalSum):-
	NewSum is SoFarSum + 1,
	NewIndex is Index + 1,
	process_solution(Ss,NewIndex,[Index|SoFarClique],FinalClique,NewSum,FinalSum).
	
	
process_solution([0|Ss],Index,SoFarClique,FinalClique,SoFarSum,FinalSum):- 
	NewIndex is Index + 1,  
	process_solution(Ss,NewIndex,SoFarClique,FinalClique,SoFarSum,FinalSum).


   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Finds all non existing arcs in the graph
find_non_connected(Graph,NNodes,NonConnected):-
	findall(X-Y,(between(1,NNodes,X),between(1,NNodes,Y),X\=Y,\+member(X-Y,Graph),\+member(Y-X,Graph)),NonConnected).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Z is a number between X and Y
between(X,Y,X):-
	X=<Y.
between(X,Y,Z):-
	X<Y,
	X1 is X + 1,
	between(X1,Y,Z).
	
	
