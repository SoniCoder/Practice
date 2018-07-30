-module(solution).
-export([main/0]).

  
expr(X, 10, Acc) -> Acc;
expr(X, T, Acc) ->
    XD = foldl(((fun(E, Acc0) -> E * Acc0), 1, [1,2,3,4,5]),
    expr(X, T+1, Acc + math:pow(X, T)/XD).


main() ->
    {N, _} = string:to_integer(string:chomp(io:get_line(""))),

    lists:foreach(fun(NItr) ->
        {X, _} = string:to_float(string:chomp(io:get_line(""))),
        io:format("~p~n", [expr(X, 1, 1)])
                  end,
        
    lists:seq(1, N)),

    ok.
