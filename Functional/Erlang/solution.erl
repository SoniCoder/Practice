% Enter your code here. Read input from STDIN. Print output to STDOUT
% Your class should be named solution

-module(solution).
-export([main/0]).

zip(L1,L2) -> t_zip(L1,L2,[]).

t_zip([H1|T1],[H2|T2],L) -> t_zip(T1,T2,[{H1,H2}|L]);
t_zip([],_,L) -> L;
t_zip(_,[],L) -> L.

main() ->
    L1 = [1,2,3,4,5],
    L2 = [7,8,9],
    zip(L1, L2).
        