{application, phonebook,  %%% app name is phonebook
  [ %%% list of key value pairs
    {description, "Phonebook app"},  %%% description of application
    {vsn, "0.0.1"}, %%% version
    {registered, [phonebook, phonebook_sup, phonebook_serv]}, %%% list of registered process names
    {applications, [ %%% list of apps that are required to be run before this app
      kernel,
      stdlib
    ]},
    {mod, {phonebook, []}}, %%% app module which is used to start the app
    {env, []} %%% configuration params
  ]}.
