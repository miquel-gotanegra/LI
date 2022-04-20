%AQUI HI HA ELS EXERCICIS FETS A CLASSE DE LABO
same_parity(X,Y):- X1 is mod(X,2), Y1 is mod(Y,2), write(X1), write(Y1), X1 == Y1. 

%pert(X,L) = "X pertenece a la lista L"      /*(en realidad ya existe en swi Prolog, y se llama member)*/
pert(X, [X|_] ).
pert(X, [_|L] ):- pert(X,L).


%concat(L1,L2,L3) = "L3 es la concatenacion de L1 con L2"                     ya existe y se llama append
concat( [],     L,  L       ).
concat( [X|L1], L2, [X|L3] ):- concat( L1, L2, L3).

% aritmetica!!
% Var is Expresion   = "unifica el resultado de evaluar Expresion con Var"

%fact(N,F) = "F es el factorial de N"  F será  N * (N-1) * ... * 1
fact(0,1):- !. %importante el ! para que no empile la clausula de abajo en cuanto llegue a 0
fact(N,F):- N1 is N-1,  fact(N1,F1), F is N * F1.
% en fact(3,F):  para la 3a llamada recursiva con fact(0,...), usa la cláusula 1 y empila la 2!!

%long(L,N) = "la longitud de L es N"                     ya existe y se llama length
long([],0).
long([_|L],N):- long(L,N1), N is N1+1.
%length() --> ya definida

%permutacion(L,P) = "P es una permutacion de la lista L"     n!                     ya existe y se llama permutation

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


%cifras( L, N ) escribe las maneras de obtener N a partir de + - * /      de los elementos de la lista L
% ejemplo:
% ?- cifras( [4,9,8,7,100,4], 380 ).
%    4 * (100-7) + 8         <-------------
%    ((100-9) + 4 ) * 4
%    ...

cifras(L,N):-
    subcjto(L,S),         % S = [4,8,7,100]
    permutation(S,P),     % P = [4,100,7,8]
    expresion(P,E),       % E = 4 * (100-7) + 8 
    N is E,
    write(E), nl, fail.


% E = ( 4  *  (100-7) )    +    8
%            +
%          /   \
%         *     8
%        / \
%       4   -
%          / \
%        100  7


expresion([X],X).
expresion( L, E1 +  E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ).
expresion( L, E1 -  E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ).
expresion( L, E1 *  E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ).
expresion( L, E1 // E2 ):- append( L1, L2, L), 
			  L1 \= [], L2 \= [],
			  expresion( L1, E1 ),
			  expresion( L2, E2 ),
                          K is E2, K\=0.              % evitamos que se produzcan divisiones por cero

% der( Expr, Var, Der )  == "la derivada de Expr con respecto Var es Der"
der( X, X, 1):- !.
der( C, _, 0):- atomic(C).     % atomic significa que es una expresion constante o un entero
der( A+B, X, U+V ):- der(A,X,U), der(B,X,V). 
der( A*B, X, A*V+B*U ):- der(A,X,U), der(B,X,V). 
% ...


% union( L1, L2, U ) == "U es la union de L1 con L1 (como conjuntos, sin repeticiones)"
%union( [],     L,  L ).
%union( [X|L1], L2, U     ):-     member(X,L2),   union( L1, L2, U ).
%union( [X|L1], L2, [X|U] ):- not(member(X,L2)),  union( L1, L2, U ).


% "not" es "negacion por fallo finito"  "negation by finite failure"
% en realidad el "not" ya está definido en swiprolog, pero si lo tuviéramos que definir, "minot", sería así:
minot( X ):- call(X), !, fail.       %minot( X ):-  X, !, fail.   <--- esto daría error sintáctico; por eso existe el "call"
minot( _ ).


%2 prod (L,P) "P es el producto de los elementos de L"
prod([X|L],P) :- prod(L,P1), P is P1*X. % P = P1*X. si fem aixo sera igual a la expressio no al resultat
prod([],1).

%3 pescalar(L1,L2,P) “Pes el producto escalar de los dos vectores L1 y L2” la suma del producto entre la misma componente

pescalar([],[],0).
pescalar([X|L1],[Y|L2],P) :- pescalar(L1,L2,Px), P is X*Y+Px.

%4 union(L1,L2,L) "L es la union de conjutos de L1 y L2 (sin repeticiones)"
union( [],     L,  L ).
union( [X|L1], L2, U     ):-     member(X,L2),   union( L1, L2, U ).
union( [X|L1], L2, [X|U] ):- not(member(X,L2)),  union( L1, L2, U ).

%  intersection(L1,L2,I) "I es la interseccion de los conjuntos L1 y L2"

intersection([],_,[]).
intersection([X|L1],L2,[X,I]):- member(X,L2),!,intersection(L1,L2,I).
%la exclamacion es para que no haga la siguiente regla en caso de que X pertenezca a L2
intersection([_|L1],L2,I) :- intersection(L1,L2,I).

%5 ultimo elemento de una lista
ultimo(L,X):- concat(_,[X],L).

%  inverso

inverso([],[]).
inverso(L,[X|L1]):-   %basicament es si X es lultim element de L i L-X es la inversa de L1
	concat(L2,[X],L), %vol dir que L = L2 . X
 	inverso(L2,L1).

%6 fib(N,F) "F es el N-essimo numero de fibonacci"
fib(1,1).
fib(2,1).
fib(N,F):- 
	N > 2, N1 is N-1, N2 is N-2, 
	fib(N1,F1), fib(N2,F2),
	F is F1 + F2.

%7 dados(P,N,L) "la lista L expresa una manera de sumar P puntos lanzando N dados"

dados(0,0,[]).
dados(P,N,[X|L]) :-
	N>0,
	pert(X,[1,2,3,4,5,6]),
	Q is P-X,
	M is N-1, dados(Q,M,L).

%concat([],L,L).
%concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).
% --> append() ya definida

%pert(X,[X|_]).
%pert(X,[_|Y]) :- pert(X,Y). 
% member() --> ya definida

pert_con_resto(X,L,R) :- concat(L1,[X|L2],L), concat(L1,L2,R).  

% permutacion([],[]).
% permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).
% -->permutation()

% long([],0).
% long([_|L],M) :- long(L,N), M is N+1.
% -->length()

% subcjto([],[]). 
% subcjto([_|C],S) :- subcjto(C,S). 
% subcjto([X|C],[X|S]) :- subcjto(C,S).
% -->subset() 

%suma(L,S) S es la suma de tots els temes de L
suma([],0).
suma([X|L],S):- suma(L,S1), S is S1 + X.

%8 suma_demas(L) "dada una lista de enteros L, se satisface si algun elemento es igual a la suma de totdos los demas"
suma_demas(L) :- pert_con_resto(X,L,R), suma(R,X), !.

%9 suma_ant(L) da cierto si algun elemento de L es la suma de sus anteriores
suma_ants(L) :- concat(L1,[X|_],L), suma(L1,X), !.


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
    
	
%10 card(L) dada una lista de enteros L, escribe cuantas veces aparece este

car([],[]).
car( [X|L] , [ [X,N1] |Cr] ):-car(L,C),pert_con_resto([X,N],C,Cr),!,N1 is N+1.
car( [X|L] , [ [X,1]   |C] ):-car(L,C).

card(L):-car(L,C),write(C).

%11 esta_ordenada(L) nos dice si L esta ordenada de menor a mayor

esta_ordenada([]).
esta_ordenada([_]) :- !.
% Este corte no es necesario estrictamente, sólo por GPROLOG.
esta_ordenada([X,Y|L]) :- X =< Y, esta_ordenada([Y|L]).

%12 oredenacion(L1,L2) L2 es L1 ordenada de mayor a menor
ordenacion(L1,L2) :- permutacion(L1,L2), esta_ordenada(L2).  

% Ejercicio 13 
% Sea n la longitud de la lista L. 

% esta_ordenada(L) tiene complejidad lineal, ya que se hacen n-1 comparaciones 
% en el peor caso (cuando la lista L está ordenada).
% 
% En el peor caso el predicado ordenacion(L,LO) tiene que generar todas las 
% permutaciones (hay n!) y comprobar que si están ordenadas. Por tanto en el  
% peor caso el coste es (n!)n < (n+1)!, donde n es la longitud de la lista L. 
% Complejidad factorial en n+1.
% Nótese que n! crece muy rápido: 2^n < n! a partir de n=4.

%--------------------------------------------------------------------------
% Ejercicios 14 y 15
% Sea n la longitud de la lista.

% Insertar un elemento en una lista ordenada tiene complejidad lineal, ya 
% que en el peor caso hay que compararlo con los n elementos de la lista.

% Ordenación por inserción inserta el último elemento en la lista vacía, 
% el penúltimo en una lista de un elemento, y así sucesivamente. Se hacen 
% 1 + 2 + ... + n-1 = n(n-1)/2 comparaciones en el peor caso. Complejidad 
% cuadrática.

% En el mejor caso (la lista ya está ordenada) la complejidad es lineal. 

%14 ordenacion(L1,L2) pero usando insercion(X,L1,L2) 
%que significa L2 es L1 con X inserido donde toca en orden ascendente

insercion(X,[],[X]).
%insercion(X,[Y],L):- X =< Y,append([X],[Y],L).
%insercion(X,[Y],L):- X > Y, append([Y],[X],L).
%insercion(X,L,R):- concat(L1,[N, K|L2],L), N =< X , X =< K, concat(L1,[N,X,K|L2],R).
insercion(X,[Y|L],[X,Y|L]) :- X=<Y. 
insercion(X,[Y|L],[Y|L1]) :- X>Y, insercion(X,L,L1). 


ord([],[]). 
ord([X|L],L1) :- ord(L,L2), insercion(X,L2,L1).

% Ejercicios 14 y 15
% Sea n la longitud de la lista.

% Insertar un elemento en una lista ordenada tiene complejidad lineal, ya 
% que en el peor caso hay que compararlo con los n elementos de la lista.

% Ordenación por inserción inserta el último elemento en la lista vacía, 
% el penúltimo en una lista de un elemento, y así sucesivamente. Se hacen 
% 1 + 2 + ... + n-1 = n(n-1)/2 comparaciones en el peor caso. Complejidad 
% cuadrática.

% En el mejor caso (la lista ya está ordenada) la complejidad es lineal. 


%16 ordenacion (L1,L2) basadao en merge sort : si la lista es de tamaño mayor a 1
%concat divide la lista en dos mitades, las ordena i luego las las fusiona de forma ordenadas

split([],[],[]).
split([A],[A],[]).
split([A,B|R],[A|Ra],[B|Rb]) :-  split(R,Ra,Rb).

merge_sort([],[])   :- !.
merge_sort([X],[X]) :- !.
merge_sort(L,L3) :- split(L,L1,L2), merge_sort(L1,L11), merge_sort(L2,L22), 
  merge(L11,L22,L3). 
  
merge(L, [], L) :- !.
merge([], L, L).
merge([X|L1],[Y|L2],[X|L3]) :- X=<Y, !, merge(L1,[Y|L2],L3). % regla adecuada 
merge([X|L1],[Y|L2],[Y|L3]) :- merge([X|L1],L2,L3). 

%17 diccionario(A,N) dado un alfabeto a  y un natural N escriba todas las palabras de N simbolos
%por orden alfabetico de A dado

%diccionario( [ga,chu,le],2)
%escribir ́a:gaga gachu gale chuga chuchu chule lega lechu lele.

diccionario(A,N):-  nperts(A,N,S), escribir(S), fail.

nperts(_,0,[]):-!.
nperts(L,N,[X|S]):- pert(X,L), N1 is N-1, nperts(L,N1,S).

escribir([]):-write(' '),nl,!.
escribir([X|L]):- write(X), escribir(L).


%18 palindromos
%Ejemplo:palindromos([a,a,c,c]) escribe[a,c,c,a] y [c,a,a,c].

palindromos(L) :- permutacion(L,P), es_palindromo(P), 
  write(P), nl, fail. 
palindromos(_). 

es_palindromo([]).
es_palindromo([_]) :- !. % regla adecuada
es_palindromo([X|L]) :- concat(L1,[X],L), es_palindromo(L1). 

% Si no queremos que escriba repetidos se puede usar setof. 
palindroms(L) :- setof(P,(permutation(L,P), es_palindromo(P)),S), write(S).

%19
/*
¿Qu ́e 8 d ́ıgitos diferentes tenemos que asignar a las letrasS,E,N,D,M,O,R,Y, 
de manera que se cumpla la sumaSEND+MORE=MONEY? 
Re-suelve el problema en Prolog con un predicadosumaque sume listas de 
d ́ıgitos.El programa debe decir “no” si no existe soluci ́on.

*/

suma([],[],[],C,C).
suma([X1|L1],[X2|L2],[X3|L3],Cin,Cout) :-
	X3 is (X1 + X2 + Cin) mod 10,
	C  is (X1 + X2 + Cin) //  10,
	suma(L1,L2,L3,C,Cout).


send_more_money1 :-

	L = [S, E, N, D, M, O, R, Y, _, _],
	permutacion(L, [0,1,2,3,4,5,6,7,8,9]),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.


send_more_money2 :-

	L = [0,1,2,3,4,5,6,7,8,9],
	pert_con_resto(M,  [0,1], _),
	pert_con_resto(M,  L,  L0),
	pert_con_resto(O, L0, L1),
	pert_con_resto(R, L1, L2),
	pert_con_resto(Y, L2, L3),
	pert_con_resto(S, L3, L4),
	pert_con_resto(E, L4, L5),
	pert_con_resto(N, L5, L6),
	pert_con_resto(D, L6, _),
	suma([D, N, E, S], [E, R, O, M], [Y, E, N, O], 0, M),

	write('S = '), write(S), nl,
	write('E = '), write(E), nl,
	write('N = '), write(N), nl,
	write('D = '), write(D), nl,
	write('M = '), write(M), nl,
	write('O = '), write(O), nl,
	write('R = '), write(R), nl,
	write('Y = '), write(Y), nl,
	write('  '), write([S,E,N,D]), nl,
	write('  '), write([M,O,R,E]), nl,
	write('-------------------'), nl,
	write([M,O,N,E,Y]), nl.