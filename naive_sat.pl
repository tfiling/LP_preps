/**
 * **********************************************
 * @file naive_sat.pl
 * 
 * A naive SAT solver.
 * 
 * This solver contains two predicates:
 * 1. is_cnf/1
 * 2. sat/1
 * see documentation below
 * 
 * if you want to use is_cnf/1 and sat/1 directly 
 * from the interactive Prolog prompt, call:
 * ?- use_module(sat).
 * true.
 * **********************************************
 * @author Michael Frank
 * **********************************************
 */
:- module(naive_sat, [is_cnf/1, sat/1]).

/*
 * **********************************************
 * is_cnf(@CNF)
 * 
 * test if term is a proper cnf.
 * **********************************************
 */
is_cnf(CNF) :-
	is_list(CNF),
	maplist(is_clause, CNF).

is_clause(Clause) :-
	is_list(Clause),
	maplist(is_literal, Clause).

is_literal( X) :- var(X) , !.
is_literal( X) :- X ==  1, !.
is_literal( X) :- X == -1, !.
is_literal(-X) :- is_literal(X).

error:has_type(cnf, CNF) :-
	is_cnf(CNF).

/*
 * **********************************************
 * sat(+CNF)
 * sat(+CNF, +Opts)
 * 
 * given a CNF assign values to its variables so
 * that it is satisfied.
 * 
 * if the CNF is satisfiable then sat/1 will 
 * assign values to its variables. Otherwise,
 * if CNF is not satisfiable -- then sat/1 will
 * fail.
 * **********************************************
 */
sat(CNF) :-
	sat(CNF, []).

sat(CNF, Opts) :-
	writeln('% simple SAT-Solver'),
	must_be(cnf, CNF),
	maplist(list_to_set, CNF, CNF0),
	cnf_simplify(CNF0, CNF1),
	once(memberchk(vars(Vars), Opts) ; term_variables(CNF1, Vars)),
	(   memberchk(randomize_vars, Opts)
	->  random_permutation(Vars, Vars0)
	;   Vars0 = Vars
	),
	sat_(Vars0, CNF1).

/*
 * **********************************************
 * sat_(+Vars, +CNF)
 *
 * given a CNF and its variables -- assign values
 * to variables (according to the ordering of Vars)
 * so that CNF is satisfied.
 * **********************************************
 */
sat_([]  ,  _) :-
	!.
sat_(Vars, []) :-
	maplist(var, Vars), !,
	maplist(=(-1), Vars).
sat_(_, [[]|_]) :-
	!, fail.
sat_([X|Xs], CNF) :-
	ground(X), !,
	sat_(Xs, CNF).
sat_([X|Xs], CNF) :-
	% writeln(vars=[X|Xs]),
	must_be(var, X),
	member(X, [-1,1]),
	cnf_simplify(CNF, CNF0),
	sat_(Xs, CNF0).

/*
 * **********************************************
 * cnf_simplify(+CNF, ?Simpl)
 * **********************************************
 */
cnf_simplify(CNF, Simpl) :-
	maplist(maplist(lit_simplify), CNF, CNF0),
	partition_cnf(CNF0, Unit, True, False, Rest),
	cnf_simplify(CNF0, Unit, True, False, Rest, Simpl).
	
cnf_simplify(CNF, [], [], [], CNF, Simpl) :-
	!, Simpl = CNF.
cnf_simplify(_, Unit, _, False, Rest, Simpl) :-
	maplist(lit_assign_true, Unit),
	maplist(clause_remove_false, False, NoFalse),
	append(NoFalse, Rest, Simpl0),
	partition_cnf(Simpl0, Unit0, True0, False0, Rest0),
	% writeln(5=partition_cnf(_, Unit0, True0, False0, _)),
	cnf_simplify(Simpl0, Unit0, True0, False0, Rest0, Simpl).

partition_cnf([], [], [], [], []).
partition_cnf([[C]|CNF], [C|Unit], HasTrue, HasFalse, Rest) :-
	!, partition_cnf(CNF, Unit, HasTrue, HasFalse, Rest).
partition_cnf([C|CNF], Unit, [C|HasTrue], HasFalse, Rest) :-
	member(X, C), ground(X), X =:= 1, !,
	partition_cnf(CNF, Unit, HasTrue, HasFalse, Rest).
partition_cnf([C|CNF], Unit, HasTrue, [C|HasFalse], Rest) :-
	member(X, C), ground(X), X =:= -1, !,
	partition_cnf(CNF, Unit, HasTrue, HasFalse, Rest).
partition_cnf([C|CNF], Unit, HasTrue, HasFalse, [C|Rest]) :-
	partition_cnf(CNF, Unit, HasTrue, HasFalse, Rest).

lit_simplify(Lit, Simpl) :-
	ground(Lit), !, Simpl is Lit.
lit_simplify(Lit, Simpl) :-
	lit_simplify(Lit, +, Simpl).

lit_simplify(Var, Sign, Lit) :-
	var(Var), !,
	(   Sign == +
	->  Lit =  Var
	;   Lit = -Var
	).
lit_simplify(-Acc, Sign, Lit) :-
	(   Sign == +
	->  lit_simplify(Acc, -, Lit)
	;   lit_simplify(Acc, +, Lit)
	).

lit_assign_true(Lit) :-
	ground(Lit), !, Lit =:= 1.
lit_assign_true(Lit) :-
	var(Lit), !, Lit = 1.
lit_assign_true(-Lit) :-
	var(Lit), !, Lit = -1.
lit_assign_true(X) :-
	throw(x(X)).
	
clause_remove_false(Clause, NoFalse) :-
	exclude(is_lit_false, Clause, NoFalse), !.
% clause_remove_false(Clause, NoFalse) :-
% 	throw(asdasdasD).

is_lit_false(Term) :-
        ground(Term), Term =:= -1.
