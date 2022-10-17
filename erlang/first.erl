-module(first).
-export([addElement/2, sortList/2, sortList/3, sortiranje/2]).

% napraviti funkciju koj dodaje eleent u listu

addElement(Lista, Broj) ->
    Lista ++ [Broj].

sortList(Lista, Broj) ->
    sort(Lista, Broj, 1).

sort(Lista, Broj, N) ->
    if
        length(Lista) < N ->
            Lista ++ [Broj];
        true ->
            Br = lists:nth(N, Lista),

            if
                Br > Broj ->
                    {One, Two} = lists:split(N-1, Lista),
                    One ++ [Broj] ++ Two;
                true ->
                    sort(Lista, Broj, N + 1)
            end
    end.


sortList(Lista, Broj, Krit) ->
    sort(Lista, Broj, 1, Krit).

sort(Lista, Broj, N, Krit) ->
    if
        length(Lista) < N ->
            Lista ++ [Broj];
        true ->
            Br = lists:nth(N, Lista),
            Cond = Krit(Br, Broj),

            if
                Cond ->
                    {One, Two} = lists:split(N-1, Lista),
                    One ++ [Broj] ++ Two;
                true ->
                    sort(Lista, Broj, N + 1, Krit)
            end
    end.

sortiranje(Lista, Broj) ->
    lists:sort(Lista ++ [Broj]).
