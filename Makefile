compile:
	rebar3 shell

#run:
#	erl -pa ./ebin -s cellphone start

run:
	erl \
		-pa ./_build/default/lib/cellphone/ebin \
		-pa ./_build/default/lib/cowboy/ebin \
		-pa ./_build/default/lib/cowlib/ebin \
		-pa ./_build/default/lib/ranch/ebin \
		-I include \
		-mnesia dir ../cellphone_data \
