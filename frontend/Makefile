BIN = bin/
ERLANG_GPB = dependencies/gpb/include/
PA_ZMQ = dependencies/erlzmq2/ebin/
PA_FRONT = bin/

all: compile

compile: Frontend

Frontend: ./*.erl
	@dependencies/gpb/bin/protoc-erl -maps -o ./ ../protos/clientProtos.proto
	@erlc -I $(ERLANG_GPB) -o $(BIN) ./clientProtos.erl
	@erlc -o $(BIN) ./frontend.erl
	@echo Erlang Compiled
	@echo Compilation done

run_frontend:
	erl -pa $(PA_FRONT) -pa $(PA_ZMQ)

clean:
	@rm -rf bin/*

install:
	@mkdir -p $(BIN)
	@echo Creating dirs
