-module(messages).
-compile(export_all).

ping() -> 
    receive
        bye -> io:format("terminate~n");
        X   -> io:format("~s~n", [X]), 
               ping()
    end.


