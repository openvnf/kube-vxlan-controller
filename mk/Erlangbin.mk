include mk/Erlanglib.mk

PREFIX = usr/local

BINDIR = bin
BIN_PATH = $(DESTDIR)/$(PREFIX)/$(BINDIR)
BIN_PATH_IN = $(BASE_PATH)/bin

install:
	mkdir -p $(BIN_PATH)
	install -p $(BIN_PATH_IN)/$(PROJECT) $(BIN_PATH)

uninstall:
	rm -f $(BIN_PATH)/$(PROJECT)
	rmdir -p $(BIN_PATH) 2>/dev/null || true

run:
	$(BIN_PATH_IN)/$(PROJECT) run

join:
	erl \
		-start_epmd false \
		-remsh $(PROJECT)@localhost \
		-sname $(PROJECT)-$$RANDOM \
		-setcookie $(PROJECT)

erlang-compile: compile
	$(REBAR) erlang
	$(REBAR) unlock

erlang-clean:
	rm -rf _build/default/erl

erlang-install:
	$(MAKE) -C _build/default/erl install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)

docker-ready: clean compile install \
              erlang-clean erlang-compile erlang-install
