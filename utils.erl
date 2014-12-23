-module(utils).

% Export a list of functions
-export([quicksort/1, fib/1, sum/1, sublist/3, max/2, min/2, at/2, rev/1, len/1, filter/2, last/1, even/1]).

%% quick sort, use the list comprehension construct.
quicksort([]) -> [];
quicksort([Head | Tail]) -> 
    quicksort([L || L <- Tail, L =< Head]) 
    ++ [Head | quicksort([R || R <- Tail, R > Head])]. 


%% accumulate all elements in a list.
sum(L) -> sum(L, 0).

% define a recursive auxilary function sum/2 for the function sum/1.
sum([], N) -> N;
sum([H|T], N) -> sum(T, H+N).


%% use of guards "when", pattern matching follows the order of expressions.
max(X, Y) when X > Y -> X;
max(_, Y) -> Y.


min(X, Y) -> 
    if 
      X < Y -> X;
      true  -> Y
    end.


%% 
filter(_, []) -> [];
filter(P, [H|T]) -> 
    case P(H) of
        true  -> [H | filter(P, T)];
        false -> filter(P, T)
    end.


%% define a function with another function.
even(L) -> filter( fun(X) -> X rem 2 == 0 end, L).


%% return the last element of a list.
last([])    -> nil;  % the null empty is nil 
last([E])   -> E;
last([_|T]) -> last(T).


%% get the element of the list L at the index of I
at(_, [])     ->  nil; 
at(I, [H|T])  -> 
    if 
      I == 1 -> H;
      I >  1 -> at(I-1, T);
      true   -> nil    % index I should NOT be less than 1.
    end.


%% reverse a list 
rev([])    -> [];
rev([H|T]) -> rev(T) ++ [H].


%% length of list 
len([])    -> 0;
len([_|T]) -> 1 + len(T).


%% get a sublist from a list given the start and end indice. 

sublist(_, _, [])    -> [];
sublist(S, E, [H|T]) -> 
    if
      S > 1 -> sublist(S-1, E-1, [H|T]);
      true  -> 
               if 
                 E > 0 -> [H | sublist(S, E-1, T)];
                 true  -> []
               end
    end.


%% calculate the fibonacci numbers 
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(1, 1, N-1). 
fib(_,  F2, 0) -> F2;
fib(F1, F2, N) -> fib(F2, F1+F2, N-1).

    % a recursive solution with terrible performance.
    %if  
    %  N < 0 -> nil;
    %  true  -> fib(N-1) + fib(N-2)
    %end.













