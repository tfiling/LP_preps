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

% part 2

diff(Xs, Ys, CNF) :-
    length(Xs, N),
    direct(Xs, N, CNF1),
    direct(Ys, N, CNF2),
    both_not_one(Xs, Ys, CNF3),
    append([CNF1, CNF2, CNF3], CNF).

both_not_one([X | Xs], [Y | Ys], CNF) :-
    both_not_one(Xs, Ys, CNF1),
    append([[[-X, -Y]], CNF1], CNF).

both_not_one([], [], []).

% gen_diff(A, B) :-
%     length(A, 3), 
%     length(B, 3), 
%     diff(A, B, CNF),
%     sat(CNF).

