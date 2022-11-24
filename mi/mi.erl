-module(mi).
-author("sbolsec").

-export([add_missing_primes/1]).

add_missing_primes(List) ->
    lists:sort(sets:to_list(sets:from_list(List ++ generate_primes_list(lists:max(List))))).

generate_primes_list(MaxNum) when MaxNum =< 0 ->
    [];
generate_primes_list(MaxNum) when MaxNum == 1 ->
    [1];
generate_primes_list(MaxNum) when MaxNum == 2 ->
    [1, 2];
generate_primes_list(MaxNum) ->
    IsPrime = check_prime(MaxNum),
    if
        IsPrime == 2 -> generate_primes_list(MaxNum - 1) ++ [MaxNum];
        true -> generate_primes_list(MaxNum - 1)
    end.

check_prime(Num) when Num =< 0 ->
    0;
check_prime(Num) when Num == 1, Num == 2 ->
    2;
check_prime(Num) ->
    List = lists:seq(1, Num),
    A = [X || X <- List, Num rem X == 0],
    length(A).
