erl -make
erl -env ERL_LIBS "." -eval "all_tests:all_tests()."
