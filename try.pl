:- use_module(naive_sat).

% 1a

nanogram_verify(Ns, N, Xs) :-
    length(Xs, N),
    rec_match(Ns, Xs).

rec_match([N | Ns], [1 | Xs]) :-
    N > 0,
    N1 is N-1,
    rec_match([N1 | Ns], Xs).

rec_match([0, N | Ns], [1 | Xs]) :-
    N > 0,
    N1 is N-1,
    rec_match([N1 | Ns], Xs).

rec_match(Ns, [0 | Xs]) :-
    Ns \= [0],
    rec_match(Ns, Xs).

rec_match([0], [0 | Xs]) :-
    rec_match([0], Xs).

rec_match([0], []).


ver_nano(A) :-
    A = [0, 1, 0],
    length(A, N),
    Ns = [1],
    nanogram_verify(Ns, N, A).

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

% 2b with B

% gen_diff(A, B) :-
%     length(A, 3), 
%     length(B, 3), 
%     diff(A, B, CNF),
%     sat(CNF).


% 2c

allDiff(XXs, N, CNF) :-
    diff_between_all(XXs, N, CNF).
    

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

gen_all_diff(A, B, C) :-
    length(A, 3), 
    length(B, 3), 
    length(C, 3), 
    allDiff([A, B, C], 3, CNF),
    sat(CNF).

% 3a
% first implementation with possible bug, probably because diff
lex(Xs, Ys, CNF) :-
    lex(_, Xs, Ys, CNF).

lex(B, [X | Xs], [Y | Ys], CNF) :-
    lex(B1, Xs, Ys, CNF1),
    append([[B, X, Y, -B1], [B, -X, -Y, -B1], [-B, B1], [-B, -X, Y], [-B, X, Y]],
            CNF1, CNF).

lex(1, [], [], []).

gen_lex1(A, B) :-
    length(A, 3),
    length(B, 3),
    lex(_, A, B, CNF),
    sat(CNF).


% second implemntation, seems more solid but less effecient
% lexLT(Xs, Ys, CNF) :-
%     lexLT2(Xs, Ys, CNF1),
%     diff(Xs, Ys, CNF2),
%     append(CNF1, CNF2, CNF).

% lexLT2([X | Xs], [Y | Ys], CNF) :-
%     lexLT2(Xs, Ys, CNF1),
%     append([[-X, Y]], CNF1, CNF).
% lexLT2([], [], []).


lexLT(Xs,Ys,Cnf):-
        lexLT2(Xs,Ys,Cnf1),
        diff(Xs,Ys,CNF2),
        append(Cnf1,CNF2,Cnf).   

lexLT2([], [], []).
lexLT2([X|Xs], [Y|Ys], Cnf):-
     lexLT(Xs, Ys, Cnf1),
     append([[[-X, Y]], Cnf1], Cnf).

gen_lex2(A, B) :-
    length(A, 3),
    length(B, 3),
    lexLT(A, B, CNF),
    sat(CNF).


% 3b
% doesnt work well, probably because of lex

matrix_lex([X1, X2 | Rest], CNF) :-
    lex(X1, X2, CNF1),
    matrix_lex([X2 | Rest], CNF2),
    append(CNF1, CNF2, CNF).

matrix_lex([_], []).
matrix_lex([], []).

gen_mat_lex(A, B, C) :-
    length(A, 4),
    length(B, 4),
    length(C, 4),
    matrix_lex([A, B, C], CNF),
    sat(CNF).

% 4a

bit_vector(N, [1 | Rest]) :- 
    N > 0, 
    N1 is N - 1,
    bit_vector(N1, Rest).

bit_vector(N, [0 | Rest]) :- 
    N > 0, 
    N1 is N - 1,
    bit_vector(N1, Rest).

bit_vector(0, []).

% 4b

swap(X1, X2, T1, T2, [X1 | RestOriginal], [T1 | RestNew]) :-
    swap(X1, X2, T1, T2, RestOriginal, RestNew).

swap(X1, X2, T1, T2, [X2 | RestOriginal], [T2 | RestNew]) :-
    swap(X1, X2, T1, T2, RestOriginal, RestNew).


swap(X1, X2, T1, T2, [X | RestOriginal], [X | RestNew]) :-
    X \= X1,
    X \= X2,
    swap(X1, X2, T1, T2, RestOriginal, RestNew).

swap(_, _, _, _, [], []).

apply_network([comparator(X1, X2, T1, T2) | Rest], In, Out) :-
    comparator(X1, X2, T1, T2),
    swap(X1, X2, T1, T2, In, NewIn),
    apply_network(Rest, NewIn, Out).

% apply_network([comparator(X1, X2, T1, T2) | Rest], In, Out) :-
%     X1 > X2,
%     apply_network(Rest, In, Out).


apply_network([], Out, Out).

comparator(X1, X2, X2, X1):-
     X1 > X2.


comparator(X1, X2, X1, X2):-
    X1 =< X2.
    
cs([
    comparator(X1, X3, T1, T3), 
    comparator(X2, X4, T2, T4),
    comparator(T1, T2, Y1, Y2), 
    comparator(T3, T4, Y3, Y4),
    comparator(Y2, Y3, Z1, Z2)
    ], [X1, X2, X3, X4]).
    