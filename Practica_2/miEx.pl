%pert(X,L) = "X pertenece a la lista L"      /*(en realidad ya existe en swi Prolog, y se llama member)*/
%member
pert(X, [X|_] ).
pert(X, [_|L] ):- pert(X,L).

same_parity(X,Y):- X1 is mod(X,2), Y1 is mod(Y,2), write(X1), write(Y1), X1 == Y1. 

%concat(L1,L2,L3) = "L3 es la concatenacion de L1 con L2"  
%append
concat([],L,L).
concat([X|L1],L2,[X|L3]):-concat(L1,L2,L3).

%fact(N,F) = "F es el factorial de N"  F será  N * (N-1) * ... * 1
fact(0,1):-!.
fact(N,F):- N1 is N -1, fact(N1,F1), F is F1*N.

%long(L,N) = "la longitud de L es N"
%length
long([],0).
long([_|L],N):-long(L,N1), N is N1+1.

%permutacion(L,P) = "P es una permutacion de la lista L"
%permutation
permutacion([],[]).
permutacion([X|L], P):- permutacion(L,P1),     
			concat( Pa,    Pb, P1), 
			concat( Pa, [X|Pb], P).

%subcjto(L,S) = "S es un subconjunto de L"  2^n
% Si L es [e1 ... en-1 en]
%           0 ...  0    1  
%                       hay tantos subconjuntos como tiras de n bits
subcjto( [], [] ).
subcjto( [_|L],    S  ):-  subcjto(L,S).
subcjto( [X|L], [X|S] ):-  subcjto(L,S).

% union( L1, L2, U ) == "U es la union de L1 con L1 (como conjuntos, sin repeticiones)"
union([],   L,  L).
union( [X|L1], L2, [X|U] ):- not(member(X,L2)),  union( L1, L2, U ).
union( [X|L1], L2, U     ):-     member(X,L2),   union( L1, L2, U ).


%2 prod (L,P) "P es el producto de los elementos de L"

prod([],1).
prod([X|L],P):- prod(L,P1), P is P1*X .


%3 pescalar(L1,L2,P) “Pes el producto escalar de los dos vectores L1 y L2” la suma del producto entre la misma componente

pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):-pescalar(L1,L2,P1), P is P1 + (X*Y).


%4 union(L1,L2,L) "L es la union de conjutos de L1 y L2 (sin repeticiones)"
%union([],L,L).
%union([X|L1],L2,[X|U]):- not(member(X,L2)),union(L1,L2,U). 
%union([X|L1],L2,U):- member(X,L2),union(L1,L2,U). 


%  intersection(L1,L2,I) "I es la interseccion de los conjuntos L1 y L2"
intersection([],_,[]).
intersection([X|L1],L2,[X|I]):- member(X,L2),!, intersection(L1,L2,I).
intersection([X|L1],L2,I):- not(member(X,L2)), intersection(L1,L2,I).

%5 ultimo elemento de una lista
ultimo([X|[]],X):-!.
ultimo([_|L],Y):-ultimo(L,Y).
%ultimo(L,X):- concat(_,[X],L).

%  inverso
inverso([],[]).
inverso([X|L],I):- concat(I1,[X],I), inverso(L,I1).


%6 fib(N,F) "F es el N-essimo numero de fibonacci"
fib(1,1):-!.
fib(2,1):-!.
fib(N,F):- N1 is N-1, N2 is N-2, fib(N1,F1), fib(N2,F2), F is F1+F2.

%dados(P,N,L) “la lista L expresa una manera de sumar P puntos lanzando N dados”
dados(0,_,[]).
dados(P,N,[D|L]):-
    N>0,
    findall(M,between(1,6,M),Ml),
    member(D,Ml), N1 is N-1, P1 is P-D,
    dados(P1,N1,L).

%suma_demas(L) "se satisface si existe algun elemento en L que es igual a la suma de los dem ́as elementos de L"
suma_demas(L):-
    member(X,L),
    concat(L1,[X|L2],L),
    suma(L1,S1),suma(L2,S2),
    S is S1 + S2,
    X == S,
    write(X).

suma([],0).
suma([X|L],S):- suma(L,S1), S is S1+X. 

suma_antes(L):- concat(L1,[X|_],L), suma(L1,S1), S1 == X, write(X). 


/*1.- [3 points] A domino piece can be represented as a pair of integers

[X,Y].  A valid domino chain is a list of pieces of the form

      [ [X1,X2], [X2,X3], [X3,X4], ..., [XN, XN1] ]

Given a list of domino pieces L, write a Prolog predicate chain(L,R)

that succeeds if R is a valid domino chain that can be obtained from L

by only flipping its pieces (i.e. converting a piece [X,Y] into [Y,X]

if necessary). Note that we cannot reorder the pieces. Given L, the

predicate should be able to compute all possible R upon backtrack.

Example:

?- chain([[1,2],[2,1]],R), write(R), nl, fail.

[[1,2],[2,1]]

[[2,1],[1,2]]

false.

?- chain([[2,1],[2,1],[3,1],[3,4],[2,4]],R), write(R), nl, fail.

[[1,2],[2,1],[1,3],[3,4],[4,2]]

false.

?- chain([[1,2],[3,4],[2,3]],R), write(R), nl, fail.

false.

Submit a single file named p1.pl (other names will not be accepted!)
*/

chain([],[]).
chain([[X,Y]], [[X,Y]]). %%esto es necessario para que el chain de abajo no de [], ya que entonces se incumple la clasula R
chain([[X,Y]], [[Y,X]]).
chain([[X,Y]|L],[[X,Y]|R]):- chain(L,R), R = [[Y,_]|_].
chain([[X,Y]|L],[[Y,X]|R]):- chain(L,R), R = [[X,_]|_].


%subcjto( [], [] ).
%subcjto( [_|L],    S  ):-  subcjto(L,S).
%subcjto( [X|L], [X|S] ):-  subcjto(L,S).

all_chains(L):-
    subcjto(L,S),
    permutation(S,P),
    chain(P,R), write(R),nl,fail.
all_chains(_).
    