cd network-simulator
erl -make
erl -env ERL_LIBS "." -eval "network_test:all_tests()."
