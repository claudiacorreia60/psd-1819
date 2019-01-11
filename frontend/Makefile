BIN = bin/
#ERLANG_ZMQ = dependencies/erlzmq2/include
ERLANG_GPB = dependencies/gpb/include/

all: compile

compile: Frontend

Frontend: ./*.erl
	@dependencies/gpb/bin/protoc-erl -maps -o ./ ./clientProtos.proto
	@erlc -I $(ERLANG_GPB) -o $(BIN) ./clientProtos.erl
	@erlc -o $(BIN) ./frontend.erl
	@echo Erlang Compiled
	@echo Compilation done

run_frontend:
	erl -pa $(BIN) -run frontend

clean:
	@rm -rf bin/*

install:
	@mkdir -p $(BIN)
	@echo Creating dirs
