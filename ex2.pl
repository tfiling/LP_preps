%Gal Tfilin and Niv Lipetz

:- use_module(naive_sat).

% PART 1 - NanoGrams

% Task 1 - is_nanogram

is_nonogram(nonogram(N, M, ColData, RowData), Solution) :-
    is_nanogram_by_rows(N, RowData, Solution),
    transpose(Solution, TransposedSolution),
    is_nanogram_by_rows(M, ColData, TransposedSolution).

is_nanogram_by_rows(N, [RowData|RestRowData], [RowSolution|RestRowSolution]) :-
    N > 0,
    N1 is N - 1,
    verif_row_data(RowData, RowSolution),                           %verify row matches the solution
    is_nanogram_by_rows(N1, RestRowData, RestRowSolution).          %check next row

is_nanogram_by_rows(0,[],[]).                                       %iterated over all rows so finish successfully

verif_row_data(RowData, [0|RowSolution]) :-                         %current element in solution is 0, go to next element
    verif_row_data(RowData, RowSolution).

verif_row_data([RowData|RestRowData], [1|RowSolution]) :-           %current element in solution is 1, count the number of 1s and check that it equals the rowdata element
    RowData > 0,
    CountRowData is RowData - 1,
    verif_row_data([CountRowData|RestRowData], RowSolution).

verif_row_data([0|RestRowData],[0|RestRowSolution]) :-              %series of 1s matches the solution so both are 0|_ 0|_
    verif_row_data(RestRowData, RestRowSolution).

verif_row_data([0],[]).                                             %finished going through one row successfully

% Nanogram utils
transpose(Rows, []) :-
    null_rows(Rows).

transpose(Rows, [FirstCol|RestCols]) :-
    make_row(Rows, FirstCol, RestRows),
    transpose(RestRows, RestCols).

make_row([[X|RestRow]|Rows], [X|Col], [RestRow|RestRows]) :-
    make_row(Rows, Col, RestRows).

make_row([],[],[]).

null_rows([[]|Rows]) :-
    null_rows(Rows).

null_rows([]).

%-----------------------------------------------------%
% Task 2 - nonogram_solve

% heuristics => do the largest row first
% row, column, row, column
% fail if not enough space for rest of row

put_ones([1|Block]):-
	put_ones(Block).
put_ones([]).

put_zeros([0|Block]):-
	put_zeros(Block).
put_zeros([]).


%Solved by brute-force. Initially tried solving with algorithms suggested in class but had problems 
%with assigning results to 0s and 1s that were already assigned a result, so it always failed.
nonogram_solve(nonogram(N,M,ColData,RowData), Solution):-
	generate_rows(N,M,RowData,Solution),
    is_nanogram_by_rows(N, RowData, Solution),
    transpose(Solution, TSolution),
    is_nanogram_by_rows(M, ColData, TSolution).

generate_rows(N,M,[Row|RestRows],[Sol|RestSol]):-
	N > 0,
	generate_row(Row, M, Sol),
	N1 is N - 1,
	generate_rows(N1, M, RestRows, RestSol).

generate_rows(0, _, [], []).
	
generate_row(RowData, N, Sol):-
	length(Sol, N), 
	put_blocks(RowData, Sol).
	
put_blocks([RowData], Sol):-
	length(Block, RowData), 
	put_ones(Block),                    
	append(Block, Rest, Sol), 
	put_zeros(Rest).                    %fill in the rest with zeros

put_blocks([D1,D2|RowData], Sol):-
	length(Block, D1), 
	put_ones(Block),
	append(Block, [0|Rest], Sol), 
	put_blocks([D2|RowData], Rest).

put_blocks(RowData, [0|Sol]):-
	put_blocks(RowData, Sol).
	

% Algorithms for nanogram solver

% Simple box
simple_box(N, Data, Row) :-
    simple_box_left(N, Data, RowLeft),
    simple_box_right(N, Data, RowRight),
    find_intersections(RowLeft, RowRight, Row).

find_intersections([], [], []).
find_intersections([LeftElement|RowLeft], [RightElement|RowRight], [1|Row]) :-
    LeftElement == RightElement,
    LeftElement == 1,                                                           %If equal 1 then add 1 to row
    find_intersections(RowLeft, RowRight, Row).
find_intersections([LeftElement|RowLeft], [RightElement|RowRight], [0|Row]) :-
    LeftElement == RightElement,
    LeftElement == 0,                                                       %If equal 0 then don't add 1 to row
    find_intersections(RowLeft, RowRight, Row).
find_intersections([LeftElement|RowLeft], [RightElement|RowRight], [0|Row]) :-
    LeftElement \= RightElement,
    find_intersections(RowLeft, RowRight, Row).

simple_box_right(N, Data, Row) :-
    reverse(Data, DataReversed),
    simple_box_left(N, DataReversed, LeftSolution),
    reverse(LeftSolution, Row).

simple_box_left(N, Data, Row) :-
    row_sum(Data, 0, RowSum),
    length(Data, NumberOfDataElements),
    NumberOfDataElements > 0,
    NumberOfMinimalZeros is NumberOfDataElements - 1,
    RowSumWithZeros is RowSum + NumberOfMinimalZeros,
    NumberOfBoxesLeft is N - RowSumWithZeros,
    solve_simple_box_left(NumberOfBoxesLeft, Data, RowWithExtra0AtEnd),
    reverse(RowWithExtra0AtEnd, ReversedRowWithExtra0AtEnd),
    cut_first_elements(1, ReversedRowWithExtra0AtEnd, CuttedReversed),                            %solve_simple_boxes returns an extra 0 at the end of each solution
    reverse(CuttedReversed, Row).

solve_simple_box_left(NumberOfBoxesLeft, [CurrentRule|Data], [1|RestRow]) :-         %Add 1 to row because currentRule > 0
    NumberOfBoxesLeft >= 0,
    CurrentRule > 0,
    UpdatedCurrentRule is CurrentRule - 1,
    solve_simple_box_left(NumberOfBoxesLeft, [UpdatedCurrentRule|Data], RestRow).

solve_simple_box_left(NumberOfBoxesLeft, [0|Data], [0|RestRow]) :-                   %add 0 to row if finished rule (must be 0 between rules)
    NumberOfBoxesLeft >= 0,
    solve_simple_box_left(NumberOfBoxesLeft, Data, RestRow).

solve_simple_box_left(NumberOfBoxesLeft, [], [0|RestRow]) :-                         %finish all rules so add 0s to row.
    NumberOfBoxesLeft >= 0,
    UpdatedNumberOfBoxesLeft is NumberOfBoxesLeft - 1,
    solve_simple_box_left(UpdatedNumberOfBoxesLeft, [], RestRow).

solve_simple_box_left(0, [], []).

row_sum([Element|RestData], Init, Sum) :-
    Sum1 is Init + Element,
    row_sum(RestData, Sum1, Sum).

row_sum([], Init, Sum) :-
    Init = Sum.

replace_0s_in_row(I, N, [E1|Row], [E2|X], [0|Answer]) :-
    E2 = 0,
    E1 = 0,
    I1 is I + 1,
    replace_0s_in_row(I1, N, Row, X, Answer).

replace_0s_in_row(I, N, [_|Row], [_|X], [1|Answer]) :-
    I1 is I + 1,
    replace_0s_in_row(I1, N, Row, X, Answer).

replace_0s_in_row(I,N,[],[],[]) :-
    I = N.

%-----------------------------------------------------%
% Task 3 - A Ramsey Verifier

% edge(X+, Y+, Graph+, Color+) - true if X and Y vertexes are connected by an edge of Color in Graph (adjacency matrix)
% finds the relevant row according to X and checks if the Yth element is of color Color
edge(X, Y, [Row | _], Color) :- 
    X is 1,
    nth1(Y, Row, Color).

edge(X, Y, [_ | T], Color) :- 
    X > 1,
    Z is X - 1,
    edge(Z, Y, T, Color).

% clique(VertexList+, Graph+, Color+) - true if the vertexes listed in VertexList list are a clique in Graph with edges of color Color
% for each two vertices in VertexList it checks that the first two vertices connected by edge and that both recursively has an edge with the rest in VertexList
clique([], _, _).
clique([_], _, _).
clique([X1,X2|R], Graph, Color) :- 
    edge(X1, X2, Graph, Color), 
    clique([X1|R], Graph, Color), 
    clique([X2|R], Graph, Color).

% cliqueN(C-, N+, Graph+, Color+) - true if exists a C, sublist of the vertexes of size N, which is a clique of edges with color Color
% inspired from https://en.wikibooks.org/wiki/Introduction_to_Programming_Languages/Exhaustive_Searches
% checks for each sub list of size N if it is a N size clique of color Color
cliqueN(C, N, [H | T], Color) :- 
    length(H, Len),
    create_list_size_n(1, VertexList, Len),
    sublist(C, VertexList), 
    length(C, N),
    clique(C, [H | T], Color).

% sublist(SubList-, List+) - true is exists SubList which is a sublist of List where the elements order is kept
% simply used by us to generate sublists of List
% copied from https://stackoverflow.com/questions/7051400/prolog-first-list-is-sublist-of-second-list (ДМИТРИЙ МАЛИКОВ's answer)
% for each item X of the list there is a predicate which is true when X in the sublist and another predicate that is true when X is not in the sublist
sublist([], _).
sublist([X|XS], [X|XSS]) :- sublist(XS, XSS).
sublist([X|XS], [_|XSS]) :- sublist([X|XS], XSS).

% find_all_bad_cliques(S+, N+, Solution+, Clique-, Color+) -    true if exists a Clique of size greate of equal to S, 
%                                                               with edeg color of Color, N is the count of vertexes
%                                                               and Solution is the adjacency matrix
find_all_bad_cliques(S, N, Solution, Clique, Color) :-
    N > S,
    N1 is N - 1,
    find_all_bad_cliques(S, N1, Solution, Clique, Color).


find_all_bad_cliques(S, N, Solution, Clique, Color) :-
    N >= S,
    cliqueN(Clique, N, Solution, Color).

        
% true if there is a clique of color 0 with size greater or equal to S
verify_ramsey(r(S, _, N), Solution, CounterExample) :-
    find_all_bad_cliques(S, N, Solution, CounterExample, 0).

% true if there is a clique of color 1 with size greater or equal to S
verify_ramsey(r(_, T, N), Solution, CounterExample) :-
    find_all_bad_cliques(T, N, Solution, CounterExample, 1).

% always true but used only once all possibble counter examples (if exists) were shown
verify_ramsey(_, _, ramsey).


%-----------------------------------------------------%
% Task 4 - A Ramsey (Brute Force) Solver

% find ramsey(r(S, T, N), Solution)

% make_row_first_element(N+, Matrix-) - generates a random top half of adjacency matrix with -1 diagonal
% recursively creates random rows with decreasing length
make_row_first_element(N, [[-1 | Rest] | AccTail]) :-
    N > 1,
    N1 is N - 1,
    make_row_tail(N1, Rest),
    make_row_first_element(N1, AccTail).

make_row_first_element(1, [[-1]]).

% make_row_tail(N+, Row-) - generates the row in the adjacency matrix
% sets 0 or 1 (predicate for each one) as many times as the original N argument
make_row_tail(N, [0 | Rest]) :-
    N >= 1,
    N1 is N - 1,
    make_row_tail(N1, Rest).

make_row_tail(N, [1 | Rest]) :-
    N >= 1,
    N1 is N - 1,
    make_row_tail(N1, Rest).

make_row_tail(0, []).

% complete_matrix(N, HalfMatrix, ResultMatrix) completes the bottom half of the adjacency matrix with values of "_"
% for each row prepends a variable _ as long as its row is smaller than the one required
complete_matrix(N, [HalfHead | HalfTail], [Head | Tail]) :-
    length(HalfHead, Len),
    Len < N,
    complete_matrix(N, [[_ | HalfHead] | HalfTail], [Head | Tail]).

complete_matrix(N, [Head | HalfTail], [Head | Tail]) :-
    length(Head, N),
    complete_matrix(N, HalfTail, Tail).

complete_matrix(_, [], []).


% checks for all of the possible matrixes if are a valid solution for the ramsey coloring
find_ramsey(r(S, T, N), Solution) :-
    make_row_first_element(N, HalfMatrix),          
    complete_matrix(N, HalfMatrix, Solution),
    transpose(Solution, Solution),                  % a valid adjacency is symetric which means that it equal to its transpos 
                                                    % -> will fill the "_" values applied from complete_matrix
    verify_ramsey(r(S, T, N), Solution, ramsey),
    print_mat(Solution).

% print_mat(Mat+) - pretty print the matrix for your comfort :-)
print_mat([H | T]) :-
    writeln(H),
    print_mat(T).

print_mat([]).

%-----------------------------------------------------%
% Part 3: Ramsey Graphs (SAT)
%choose_n_from_k => based on the code learnt in class

create_list_size_n(N, [N | Acc], OriginalLen) :-
    N < OriginalLen,
    N1 is N + 1,
    create_list_size_n(N1, Acc, OriginalLen).

create_list_size_n(N, [N], N).

choose_k_from_n(K, N, [First|Rest]) :-
    first(K, N, First),
    last(K, N, Last),
    choose_k_from_n_aux(N, First, Last, Rest).
choose_k_from_n_aux(_, Last, Last, []).
choose_k_from_n_aux(N, Prev, Last, [Next|Rest]) :-
    increment(N, Prev, Next),
    choose_k_from_n_aux(N, Next, Last, Rest).

%find first element in kchoosen
first(K, N, First) :-
    integer(K),
    integer(N),
    K =< N,
    create_list_size_n(1, ListOfN, N),
    first(K, ListOfN, [], First).
first(K, [Element|RestListOfN], Acc, First) :-
    K > 0,
    K1 is K - 1,
    first(K1, RestListOfN, [Element|Acc], First).
first(0, _, Acc, First) :-
    Acc = First.

%find last element in kchoosen
last(K, N, Last) :-
    integer(K),
    integer(N),
    K =< N,
    create_list_size_n(1, ListOfN, N),
    NumberOfElementsToCut is N - K,
    cut_first_elements(NumberOfElementsToCut, ListOfN, List),       %cut out the N-K of the first elements
    first(K, List, [], Last).                                       %apply first on the last K elements
    

%erase first NumElementsToCut elements of list
cut_first_elements(NumElementsToCut, [_|List], CuttedList) :-
    NumElementsToCut > 0,
    NumElementsToCut1 is NumElementsToCut - 1,
    cut_first_elements(NumElementsToCut1, List, CuttedList).

cut_first_elements(0, List, CuttedList) :-
    CuttedList = List.

%increment function we use in kchoosen
increment(0,[],[]).

increment(N,[PrevNum|Next],[NextNum|Next]):-
    N > 0,
    PrevNum < N,
    NextNum is PrevNum + 1.

increment(N,[Prev1|RestPrevNum],[Next1, Next2|Next]):-
    N > 0,
    Prev1 == N,
    N1 is N - 1,
    increment(N1,RestPrevNum,[Next2|Next]),
    Next1 is Next2 + 1.                                       %increment remaining number


% list_list_vertices_to_edges(List_of_lists_of_vertices+, Matrix+, List_of_lists_of_edges-)
% recursively converts each vertices list into edges list by using list_vertices_to_edges
list_list_vertices_to_edges([H | Tail], Matrix, [HEdges | TailEdges]) :-
    list_vertices_to_edges(H, Matrix, HEdges),
    list_list_vertices_to_edges(Tail, Matrix, TailEdges).

list_list_vertices_to_edges([], _, []).

% list_list_vertices_to_edges(list_of_vertices+, Matrix+, list_of_edges-)
% using the same approach we used when checking if the vertex list is a clique - 
%   for the first two vertices add the edge connecting them and recursively add the edges between them and the rest of the list (similarly to edge predicate)
list_vertices_to_edges([X1, X2 | RestV], Matrix, [Edge | RestE1E2]) :-
    nth1(X1, Matrix, Row),
    nth1(X2, Row, Edge),
    list_vertices_to_edges([X1 | RestV], Matrix, RestE1),
    list_vertices_to_edges([X2 | RestV], Matrix, RestE2),
    append([RestE1, RestE2], RestE1E2).

list_vertices_to_edges([], _, []).
list_vertices_to_edges([_], _, []).

% create_var_matrix(N+, Matrix-) - create a matrix of size N*N filled with variables
create_var_matrix(N, Matrix):- 
    length(Matrix, N),
    rows(N, Matrix).

% rows(N+, Rows+) - each item in Rows is a list of size N (used for craete matrix)
rows(N, [X | Xs]) :-
    length(X, N),
    rows(N, Xs).

rows(_, []).

% map(N+, Map-) - creates a Map matrix to be used when decoding the solution of the SAT solver
% creates an empty matrix (filled with _ ), sets the diagonal to be 0s and applies transpose predicate, making it a valid adjacency matrix
map(N, Map) :- 
    create_var_matrix(N, Map),
    set_diagonal(0,Map,N,N,N),
    transpose(Map, Map).

% sets the main diagonal to be DiagVal
set_diagonal(_,[],RowLen,0,RowLen).
set_diagonal(DiagVal,[[DiagVal|_]|RestRows],N,N,RowLen) :-
	N1 is N - 1,
	set_diagonal(DiagVal,RestRows,RowLen,N1,RowLen).
set_diagonal(DiagVal,[[_|RestRow]|RestRows],N,M,RowLen) :-
	N \= M,
	N1 is N - 1,
	set_diagonal(DiagVal,[RestRow|RestRows],N1,M,RowLen).

% negate_matrix(Matrix+, NegMatrix-) - applies a negative wrapper on each element of the matrix (used for the creation of the part for the T edge color on the CNF)
% not something complicated, simply iterateds recursively over each item in each row and wrapps it with -1
negate_matrix([[H | T] | RestRows], [[-H | NegTail] | RestNegRows]) :-
    negate_matrix([T | RestRows], [NegTail | RestNegRows]).

negate_matrix([[] | RestRows], [[] | RestNegRows]) :-
    negate_matrix(RestRows, RestNegRows).

negate_matrix([], []).

% reverse_vertices_list(List+, ReversedList-) - reverses each vertex list in List (which should be a list of lists)
reverse_vertices_list([H | T], [HR | TR]) :-
    reverse(H, HR),
    reverse_vertices_list(T, TR).

reverse_vertices_list([], []).

% as explained in class:
%   creates a map of variables to be used for the decoding of the solution
%   generates all possible edges connecting any possible vertex list of size S and T
%   each edge in the T list is wrapped with - since this is a different coloring
%   all is appended to one CNF expression which is pretty printed for your comfort
encode_ramsey(r(S, T, N), Map, CNF) :-
    map(N, Map),
    choose_k_from_n(S, N, Vs1R),
    reverse_vertices_list(Vs1R, Vs1),
    list_list_vertices_to_edges(Vs1, Map, CNF1),
    negate_matrix(Map, NegMap),
    choose_k_from_n(T, N, Vs2R),
    reverse_vertices_list(Vs2, Vs2R),
    list_list_vertices_to_edges(Vs2, NegMap, CNF2),
    append(CNF1, CNF2, CNF),
    print_mat(CNF).

% encode the prblem to CNF, run it on SAT solver and decode the solution if exists
solve_ramsey(r(3,3,5), Solution) :-
    encode_ramsey(r(3,3,5), Map, CNF),
    sat(CNF),
    decode_ramsey(Map, Solution).
    

% replace(A, B, C, D) in mode (+, +, +, -) replaces any occurance of A with B and B with A in list C
% from our solution to ex1
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [R|T], [O|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, H \= R, replace(O, R, T, T2).

% replaces every 0 with -1 and vice versa to fit the original colored adjecency matrix definition which can be tested with out verify_ramsey predicate
decode_ramsey([Hmap | Tmap], [Hsol, Tsol]) :-
    replace(0, -1, Hmap, Hsol),
    decode_ramsey(Tmap, Tsol).

decode_ramsey([], []).