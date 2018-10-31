cellp:
	mkdir -p ./apps/cellphone/ebin \
		erl -pa ./apps/cellphone/ebin -noinput \
		-eval "case make:all() of up_to_date -> halt(0); error -> halt(1) end."

robot:
	erl -pa ./_build/default/lib/cp_robot/ebin ./_build/default/lib/cellphone/ebin ./_build/default/lib/cowlib/ebin \
		-I ./apps/cp_robot/src \
		-s cp_robot_app

# run:
# 	erl -pa ./ebin -s cellphone start

run:
	erl \
		-pa ./_build/default/lib/cellphone/ebin \
		-pa ./_build/default/lib/cowboy/ebin \
		-pa ./_build/default/lib/cowlib/ebin \
		-pa ./_build/default/lib/ranch/ebin \
		-I include \
		-mnesia dir ./cellphone_data \
		-s cellphone_app

pb:
	./_build/default/lib/gpb/bin/protoc-erl -I apps/cellphone/src/proto \
		-o-erl apps/cellphone/src/proto \
		-o-hrl apps/cellphone/src/include \
		apps/cellphone/src/proto/*.proto


