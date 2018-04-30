:- use_module(naive_sat).

% 2a

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

% 2b

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


% 2c

allDiff(XXs, N, CNF) :-
    direct_all(XXs, N, CNF1),
    diff_between_all(XXs, N, CNF2),
    append([CNF1, CNF2], CNF).

direct_all([Xs | XXs], N, CNF) :-
    direct(Xs, N, CNF1),
    direct_all(XXs, N, CNF2),
    append(CNF1, CNF2, CNF).

direct_all([], _, []).

diff_between_all([Xs1, Xs2 | XXs], N, CNF) :-
    diff(Xs1, Xs2, CNF1),
    diff_between_all([Xs1 | XXs], N, CNF2),
    diff_between_all([Xs2 | XXs], N, CNF3),
    append([CNF1, CNF2, CNF3], CNF).

diff_between_all([], _, []).
diff_between_all([_], _, []).

% gen_all_diff(A, B, C) :-
%     length(A, 3), 
%     length(B, 3), 
%     length(C, 3), 
%     allDiff([A, B, C], 3, CNF),
%     sat(CNF).