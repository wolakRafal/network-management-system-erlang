erl -make
erl -env ERL_LIBS "." -eval "eunit:test({application, network})."
