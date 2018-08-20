VERSION = $(shell cat src/$(PROJECT).app.src | grep vsn | cut -d\" -f2)
GIT_SHA = $(shell git rev-parse HEAD | cut -c1-8)

REBAR = $(shell which ./rebar3 || which rebar3)

BASE_PATH = _build/default

LIB_PATH = $(BASE_PATH)/lib
PLUGIN_PATH = $(BASE_PATH)/plugins

compile:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean-lib:
	find $(LIB_PATH) -type f \
		-name \*.beam -o -name \*.app -o -name erlcinfo | xargs rm -f
	find $(LIB_PATH) -type d \
		-name .rebar3 -o -name ebin | xargs rmdir 2>/dev/null || true

clean-plugin:
	find $(PLUGIN_PATH) -type f \
		-name \*.beam -o -name \*.app -o -name erlcinfo | xargs rm -f
	find $(PLUGIN_PATH) -type d \
		-name .rebar3 -o -name ebin | xargs rmdir 2>/dev/null || true

clean: clean-lib clean-plugin

distclean:
	rm -rf _build

shell:
	$(REBAR) shell
	$(REBAR) unlock

upgrade:
	$(REBAR) upgrade
	$(REBAR) unlock

git-release:
	git tag -a $(VERSION)
	git push origin $(VERSION)

version:
	@echo "Version $(VERSION) (git-$(GIT_SHA))"
