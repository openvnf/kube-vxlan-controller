include mk/Erlanglib.mk

PREFIX = usr/local

BINDIR = bin
BINPATH = $(DESTDIR)/$(PREFIX)/$(BINDIR)

BIN_PATH = $(BASE_PATH)/bin
ERL_PATH = $(BASE_PATH)/erl

USAGE_PADDING = 16

clean::
	rm -f $(BIN_PATH)/$(PROJECT)

install:
	mkdir -p $(BINPATH)
	install -p $(BIN_PATH)/$(PROJECT) $(BINPATH)

uninstall:
	rm -f $(BINPATH)/$(PROJECT)
	rmdir -p $(BINPATH) 2>/dev/null || true

run:
	$(BIN_PATH)/$(PROJECT) $(RUN_ARGS)

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

define usage-erlangbin-targets
	$(usage-erlanglib-targets)
	@printf '$(shell printf "    %%-$(USAGE_PADDING)s %%s\\\n%.0s" {1..8})' \
	install "Install \"$(BIN_PATH)/$(PROJECT)\" to \"$(BINPATH)\"" \
	uninstall "Remove \"$(BINPATH)/$(PROJECT)\"" \
	run "Run \"$(BIN_PATH)/$(PROJECT)\"" \
	join "Attach to the running executable Erlang machine with remote shell" \
	erlang "Create an Erlang release with runtime and libraries needed to run" \
	erlang-clean "Remove created Erlang release" \
	erlang-install \
		"Install created Erlang release to \"$(DESTDIR)/$(PREFIX)\"" \
	package-ready "Prepare for packaging"
endef

define usage-erlangbin-variables
	@printf '$(shell printf "    %%-$(USAGE_PADDING)s %%s\\\n%.0s" {1..3})' \
	RUN_ARGS "Compiled binary run arguments (current: \"$(RUN_ARGS)\")" \
	DESTDIR "Compiled binary or Erlang release installation chroot (current: \"$(DESTDIR)\")" \
	PREFIX "Compiled binary or Erlang release installation prefix (current: \"$(PREFIX)\")"
endef

define usage
	@echo "Usage: make <Target> [Variables]"
	@echo
	@echo "Targets"
	$(usage-erlangbin-targets)
	@echo
	@echo "Variables"
	$(usage-erlangbin-variables)
endef
