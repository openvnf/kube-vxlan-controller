VERSION = $(shell cat src/$(PROJECT).app.src | grep vsn | cut -d\" -f2)
GIT_SHA = $(shell git rev-parse HEAD | cut -c1-8)

REBAR = $(shell which ./rebar3 || which rebar3)

BASE_PATH = $(shell QUIET=1 $(REBAR) path --base)
PLUGIN_BEAMS = $(BASE_PATH)/plugins/*/ebin/*.beam

compile:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean:
	$(REBAR) clean -a
	rm -f $(PLUGIN_BEAMS)

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
