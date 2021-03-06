-module(utils).

% Export a list of functions
-export([quicksort/1, sort/1, fib/1, sum/1, sublist/3, max/2, min/2, at/2, rev/1, len/1, filter/2, last/1, even/1]).

-export([insert/2, lookup/2]).

%% quick sort, use the list comprehension construct.
quicksort([]) -> [];
quicksort([Head | Tail]) -> 
    quicksort([L || L <- Tail, L =< Head]) 
    ++ [Head | quicksort([R || R <- Tail, R > Head])]. 


%% more efficient than the List Comprehension, since we partition the list in one pass.
sort([])    -> [];
sort([H|T]) -> 
    {Small, Large} = partition(H, T),
    sort(Small) ++ [H | sort(Large)].


partition(Pivot, List) -> partition(Pivot, List, [], []).

partition(_, [], Small, Large) -> {Small, Large};
partition(Pivot, [H|T], Small, Large) when H >= Pivot 
        -> partition(Pivot, T, Small, [H|Large]);
partition(Pivot, [H|T], Small, Large) when H < Pivot 
        -> partition(Pivot, T, [H|Small], Large).


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
len(L)    -> len(L, 0).

len([], N)    -> N;
len([_|T], N) -> len(T, N+1).


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



%% functions about binary tree. 

%% Insert a new value into a tree. {} represents an empty node. 
insert({}, NewValue)   -> {NewValue, {}, {}};
insert(Tree, NewValue) -> 
    {Value, Left, Right} = Tree, 
    if
        Value >= NewValue -> {Value, insert(Left, NewValue), Right};
        true              -> {Value, Left, insert(Right, NewValue)}
    end.


lookup({}, _)   -> nil;
lookup(Tree, V) -> 
    {Value, Left, Right} = Tree, 
    if
        Value == V -> Value;
        Value >= V -> lookup(Left, V);
        true       -> lookup(Right, V)
    end.















