% branch_and_bound and ic libraries are used in this program 			 
% hcvrp(5, 3, 0, Solution, Cost, Time) ---> 0.81s cpu										           	
% hcvrp(7, 3, 0, Solution, Cost, Time) ---> 3.39s cpu											       
% hcvrp(8, 4, 0, Solution, Cost, Time) ---> 45.24s cpu										           
% hcvrp(12, 6, 900, Solution, Cost, Time) :                                                            
% Branch-and-bound timeout while searching for solution better than 1102957                            



:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).
:- import occurrences/3 from ic_global.

%%%%%%%%%%%%%%%%%%%%%%%% hcvrp/6 %%%%%%%%%%%%%%%%%%%%%

hcvrp(NCl, NVe, Timeout, FinalSolution, Cost, Time):-
	
	% Get starting time
	cputime(T1),
	
	% Solution is a list of lists : Each of them belongs to a vehicle.
	length(Solution,NVe),
	initialize(Solution,NCl),
	
	create_client_list(NCl,Clients),
	create_vehicle_list(NVe,Vehicles),
	create_quantity_list(Clients,Quantities),
	
	
	% Create a matrix with distances
	Length is NCl + 1,
	length(Distances,Length),
	init_matrix(Distances,Length),
	List  = [c(_,0,0)|Clients],  % Add warehouse at the beggining
	find_distances(Distances,List),
	flatten(Distances,FlatDist),
	
	% Constrain predicate
	constrain(Solution,Vehicles,Quantities,NVe,NCl),
	
	
	% Cost function
	cost(Solution,FlatDist,Length,Cost),
	
	% Flatten Solution
	flatten(Solution,FlattenedSolution),
	
	
	% Call bb_min with the appropriate timeout option
	call_bb_min(FlattenedSolution,Cost,Timeout),
	
	% Reconstruct Solution by removing zeros
	reconstruct(FlattenedSolution,NCl,FinalSolution),
	
	
	% Get finish time
	cputime(T2),
	
	Time is T2 - T1 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_bb_min(FlattenedSolution,Cost,0):-!,
	bb_min(search(FlattenedSolution, 0, first_fail, indomain_middle, complete, []),Cost, bb_options{strategy:restart}).
	 
call_bb_min(FlattenedSolution,Cost,Timeout):-
	bb_min(search(FlattenedSolution, 0, first_fail, indomain_middle, complete, []),Cost, bb_options{strategy:restart,timeout:Timeout}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize([],_).
initialize([Vehicle|Rest],NCl):-
	length(Vehicle,NCl),
	Vehicle #:: 0..NCl,
	initialize(Rest,NCl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cost(Solution,Distances,Length,Cost):-
	cost(Solution,Distances,Length,Cost,[]).

% Acc is an accumulator for the cost of each vehicle
cost([],_,_,FinalCost,Acc):-FinalCost #= sum(Acc).
cost([S|Ss],Distances,Length,FinalCost,Acc):-
	
	% Last argument is a flag indicating that we should add the distance: warehouse --> FirstClient
	% CurrentCost is the cost for the current vehicle
	find_cost(S,Distances,CurrentCost,Length,[],1),
	cost(Ss,Distances,Length,FinalCost,[CurrentCost|Acc]).

	
% Base case: Adding distance from LastClient --> warehouse
find_cost([LastClient|[]],Distances,Cost,Length,Acc,0):-
	Index #= LastClient * Length + 1,
	element(Index,Distances,Distance),
	Cost #= sum(Acc) + Distance.	
	
% Adding distance from warehouse --> FirstClient
find_cost([FirstClient|Clients],Distances,Cost,Length,Acc,1):-
	Index #= FirstClient + 1,
	element(Index,Distances,Distance),
	find_cost([FirstClient|Clients],Distances,Cost,Length,[Distance|Acc],0).
	
	
% Recursive call
find_cost([PrevClient,NextClient|Clients],Distances,Cost,Length,Acc,0):-
	Index #= PrevClient * Length + NextClient + 1,
	element(Index,Distances,Distance),
	find_cost([NextClient|Clients],Distances,Cost,Length,[Distance|Acc],0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

constrain(Solution,Vehicles,Quantities,NVe,NCl):-
	symmetrical_solution(Solution),
	% adding a dummy client(with zero quantity) at the beggining of the Quantity list
	capacity_limit(Solution,Vehicles,[0|Quantities]),
	one_occurr(Solution,NVe,NCl),
	no_leading_zeros(Solution).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

no_leading_zeros([]).
no_leading_zeros([S|Ss]):-
	check_zeros(S),
	no_leading_zeros(Ss).

check_zeros([_|[]]).
check_zeros([PrevClient,NextClient|Clients]):-
	(PrevClient #= 0)  => (NextClient #= 0),
	check_zeros([NextClient|Clients]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

symmetrical_solution([]).

symmetrical_solution([S|Ss]):-
	test_symmetry(S),
	symmetrical_solution(Ss).

	
test_symmetry([FirstClient|Clients]):-
	test_symmetry(Clients,FirstClient).

test_symmetry([_|[]],_).	

test_symmetry([PrevClient,NextClient|Clients],FirstClient):-
	(NextClient #= 0) => (FirstClient #>= PrevClient),
	test_symmetry([NextClient|Clients],FirstClient).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks capacity_limit for each vehicle		
capacity_limit([],[],_).
capacity_limit([S|Ss],[V|Vs],Quantities):-
	accumulate(S,Quantities,Acc),
	sum(Acc) #=< V,
	capacity_limit(Ss,Vs,Quantities).

accumulate([],_,[]).
accumulate([ClientIndex|Cs],Quantities,[Quantity|Acc]):-
	ClientIndex #= Index - 1,
	element(Index,Quantities,Quantity),
	accumulate(Cs,Quantities,Acc).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Makes sure that every client (1 .. NCl) will be served exactly one time
one_occurr(Solution,_,NCl):-
	flatten(Solution,FlatSol),
	Value 	#:: 1..NCl,
	get_domain_as_list(Value,DomainList),
	one_occurr(FlatSol,DomainList).
	
one_occurr(_,[]).
one_occurr(FlatSol,[Value|Rest]):-
	occurrences(Value,FlatSol,1),
	one_occurr(FlatSol,Rest).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

create_client_list(Length,List):-
	clients(Clients),
	create_client_list(Length,List,Clients,1).
	

% "I" is needed in order to check if we've reached our Length
create_client_list(Length,[C|[]],[C|_],Length).	
create_client_list(Length,[C|Ls],[C|Cs],I):-
	NewI is I + 1,
	create_client_list(Length,Ls,Cs,NewI).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

create_vehicle_list(Length,List):-
	vehicles(Vehicles),
	create_vehicle_list(Length,List,Vehicles,1).
	
	
create_vehicle_list(Length,[V|[]],[V|_],Length).	
create_vehicle_list(Length,[V|Ls],[V|Vs],I):-
	NewI is I + 1,
	create_vehicle_list(Length,Ls,Vs,NewI).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_quantity_list([],[]).		
create_quantity_list([c(D,_,_)|Clients],[D|Ds]):-
	create_quantity_list(Clients,Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

find_distances(Distances,List):-
	find_distances(Distances,List,1,1).

find_distances([],_,_,_).

find_distances([[]|Rows],List,I,_):-
	NewI is  I + 1,
	find_distances(Rows,List,NewI,1).
	
find_distances([[Distance|Cs]|Rows],List,I,J):-
	match(List,I,c(_,X1,Y1)),
	match(List,J,c(_,X2,Y2)),
	euclidean_distance(X1,Y1,X2,Y2,Distance),
	NewJ is J + 1,
	find_distances([Cs|Rows],List,I,NewJ).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

euclidean_distance(X1,Y1,X2,Y2,Result):-
	A is X1-X2,
	B is Y1-Y2,
	Sum is A * A + B * B,
	Sqrt is sqrt(Sum) * 1000,
	Result is fix(round(Sqrt)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

init_matrix([],_).
init_matrix([D|Ds],Length):-
	length(D,Length),
	init_matrix(Ds,Length).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

reconstruct([],_,[]).
reconstruct(Flat,NCl,[InnerList|Final]):-
	% create a NCl-long list for every truck
	create_inner_list(Flat,NCl,RestFlat,InnerList),
	reconstruct(RestFlat,NCl,Final).
	
create_inner_list(Flat,NCl,RestFlat,InnerList):-
	create_inner_list(Flat,NCl,1,RestFlat,InnerList).
	
% Truck-list ends with 0
create_inner_list([0|Ss],NCl,NCl,Ss,[]):-!.

% Truck-list ends with a normal client	
create_inner_list([S|Ss],NCl,NCl,Ss,[S]).


% Zero has been found
create_inner_list([0|Ss],NCl,J,RestFlat,InnerList):-!,
	NewJ is J + 1,
	create_inner_list(Ss,NCl,NewJ,RestFlat,InnerList).

% Normal client has been found
create_inner_list([S|Ss],NCl,J,RestFlat,[S|InnerList]):-
	NewJ is J + 1,
	create_inner_list(Ss,NCl,NewJ,RestFlat,InnerList).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

% Matches the n-th element of a list to the third argument
match([H|_],1,H) :-
    !.
match([_|T],N,H) :-
    N > 1, %add for loop prevention
    N1 is N-1,
    match(T,N1,H).
