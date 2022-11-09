-module(phonebook_log).
-author("sbolsec").

%% API
-export([create/0, add/2, find/2, delete/2]).

create() ->
  dict:new().

add(Phonebook, {Name, Number}) ->
  dict:store(Name, Number, Phonebook).

find(Phonebook, Name) ->
  find_key(Phonebook, Name, dict:is_key(Name, Phonebook)).

delete(Phonebook, Name) ->
  dict:erase(Name, Phonebook).

%%%%%

find_key(_, _, false) ->
  nil;
find_key(Phonebook, Name, true) ->
  dict:fetch(Name, Phonebook).
