-module(utils).

% Export a list of functions
-export([quicksort/1, sum/1, max/2, filter/2, last/1, even/1]).

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




