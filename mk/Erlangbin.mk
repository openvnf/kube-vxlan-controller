include mk/Erlanglib.mk

PREFIX = usr/local

BINDIR = bin
BIN_PATH = $(DESTDIR)/$(PREFIX)/$(BINDIR)
BIN_PATH_IN = $(BASE_PATH)/bin

ERL_PATH = $(BASE_PATH)/erl

clean: clean-lib clean-plugin
	rm -f $(BIN_PATH_IN)/$(PROJECT)

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

erlang: compile
	$(REBAR) erlang
	$(REBAR) unlock

erlang-clean:
	rm -rf $(ERL_PATH)

erlang-install:
	$(MAKE) -C $(ERL_PATH) install DESTDIR=$(DESTDIR) PREFIX=$(PREFIX)

package-ready: clean erlang-clean erlang install erlang-install
