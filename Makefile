USER = aialferov
PROJECT = kube-vxlan-controller

VERSION = $(shell cat src/$(PROJECT).app.src | grep vsn | cut -d\" -f2)
GIT_SHA = $(shell git rev-parse HEAD | cut -c1-8)

REBAR = ./rebar3

PREFIX = usr/local

BIN_DIR = bin
BIN_PATH = $(DEST_DIR)/$(PREFIX)/$(BIN_DIR)
BIN_PATH_IN = $(shell $(REBAR) path --bin)

BUILD_DIR = _build
BUILD_DIR_IMAGE = $(BUILD_DIR)/image

all:
	$(REBAR) compile
	$(REBAR) unlock

shell:
	$(REBAR) shell
	$(REBAR) unlock

upgrade:
	$(REBAR) upgrade
	$(REBAR) unlock

run:
	$(BIN_PATH_IN)/$(PROJECT) run

install:
	mkdir -p $(BIN_PATH)
	install -p $(BIN_PATH_IN)/$(PROJECT) $(BIN_PATH)

install-check:
	mkdir -p $(BIN_PATH)
	install -p scripts/check.sh $(BIN_PATH)/$(PROJECT)-check

uninstall:
	rm -f $(BIN_PATH)/$(PROJECT)
	rmdir -p $(BIN_PATH) 2> /dev/null || true

uninstall-check:
	rm -f $(BIN_PATH)/$(PROJECT)-check
	rmdir -p $(BIN_PATH) 2> /dev/null || true

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

docker-build: all
	$(MAKE) install DEST_DIR=$(BUILD_DIR_IMAGE) PREFIX=
	install -p -m 644 Dockerfile $(BUILD_DIR_IMAGE)
	docker build $(BUILD_DIR_IMAGE) -t $(USER)/$(PROJECT):$(VERSION)
	docker tag $(USER)/$(PROJECT):$(VERSION) $(USER)/$(PROJECT):edge

docker-push:
	docker push $(USER)/$(PROJECT):$(VERSION)
	docker push $(USER)/$(PROJECT):edge

docker-release:
	docker tag $(USER)/$(PROJECT):$(VERSION) $(USER)/$(PROJECT):latest
	docker push $(USER)/$(PROJECT):latest

docker-run:
	docker run --name $(PROJECT) --rm -it -v ${PWD}/pki:/pki \
		$(USER)/$(PROJECT):$(VERSION) run

docker-start:
	docker run --name $(PROJECT) --rm -d -v ${PWD}/pki:/pki \
		$(USER)/$(PROJECT):$(VERSION) run

docker-stop:
	docker stop $(PROJECT)

docker-clean:
	rm -f $(BUILD_DIR_IMAGE)/Dockerfile
	$(MAKE) uninstall DEST_DIR=$(BUILD_DIR_IMAGE) PREFIX=

docker-clean-dangling:
	docker images -qf dangling=true | xargs docker rmi

distclean: clean docker-clean
	rm -rf $(BUILD_DIR)

version:
	@echo "Version $(VERSION) (git-$(GIT_SHA))"
