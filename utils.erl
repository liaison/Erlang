-module(utils).
-export([quicksort/1]).


quicksort([]) -> 
    [];
quicksort([Head | Tail]) -> 
    quicksort([L || L <- Tail, L =< Head]) ++ 
        [Head | quicksort([R || R <- Tail, R > Head])]. 




