erl -noshell -pa ebin -eval "eunit:test(equipment_tests, [verbose])" -s init stop



Eshell V7.1  (abort with ^G)
Path = "test/resources/mit_me_1.json".
{ok, Binary} = file:read_file(Path).
Eqp = jsx:decode(Binary).

