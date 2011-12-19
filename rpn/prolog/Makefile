BIN=rpn
SRC=rpn.prolog
SWIPL=swipl
GOAL=repl

$(BIN): $(SRC)
	swipl -g $(GOAL) -t halt -o $(BIN) -c $(SRC)

clean:
	rm -f $(BIN)
