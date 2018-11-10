% ic library is being used in this exercise
% length of Statements: 10000 ~> 0.1 cpu time
% length of Statements: 100000 ~> 9.78s cpu time
% length of Statements: 200000 ~> 6.67s cpu time
% length of Statements: 300000 ~> received "stackoverflow" error


:- set_flag(print_depth, 1000).
:-lib(ic).

liars_csp(Statements,Solution):-
	length(Statements,N),
	length(Solution,N),
	Solution #:: 0..1,
	Liars #= sum(Solution),
	constrain(Statements,Solution,Liars),
	generate_input_order(Solution).
	
constrain([],[],_).
constrain([St|Sts],[S|Ss],Liars):-
	S #\= (St #=< Liars),
	constrain(Sts,Ss,Liars).

generate_input_order([]).
generate_input_order([Column|Columns]) :-
   indomain(Column),
   generate_input_order(Columns).
   
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generates random input with N length
% You may use it before calling liars_csp predicate in order to
% test liars_csp on long lists.
% e.g: ?- seed(100000), genrand(50000, C), liars_csp(C, Liars).
genrand(N, List) :-
	length(List, N),
	make_list(N, List).

	make_list(_, []).
	make_list(N, [X|List]) :-
	random(R),
	X is R mod (N+1),
	make_list(N, List).
