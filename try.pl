:- use_module(naive_sat).

direct(Xs, N, CNF) :-
    length(Xs, N),
    up_to_one_bit(Xs, CNF1),
    append([[Xs], CNF1], CNF).

up_to_one_bit([X1, X2 | Rest], CNF) :-
    up_to_one_bit([X1 | Rest], CNF1),
    up_to_one_bit([X2 | Rest], CNF2),
    append([[[-X1, -X2]], CNF1, CNF2], CNF).
    
up_to_one_bit([], []).
up_to_one_bit([_], []).

bin_gen(Xs, N) :-
    direct(Xs, N, CNF),
    sat(CNF).
