REGISTRY = docker.io

IMAGE = $(REGISTRY)/$(USER)/$(PROJECT):$(VERSION)
IMAGE_LATEST = $(REGISTRY)/$(USER)/$(PROJECT):latest

DOCKER_RUN_ARGS = -it --rm --name $(PROJECT)
DOCKER_BUILD_ARGS = . -t $(IMAGE)

DOCKER_USAGE_PADDING = 24

docker-build:
	docker build $(DOCKER_BUILD_ARGS_EXTRA) $(DOCKER_BUILD_ARGS)

docker-push:
	docker push $(IMAGE)

docker-release: docker-local-release docker-push
	docker push $(IMAGE_LATEST)

docker-local-release:
	docker tag $(IMAGE) $(IMAGE_LATEST)

docker-run:
	docker run $(DOCKER_RUN_ARGS) $(DOCKER_RUN_ARGS_EXTRA) $(IMAGE) $(RUN_ARGS)

docker-run-d:
	docker run -d $(DOCKER_RUN_ARGS) $(DOCKER_RUN_ARGS_EXTRA) $(IMAGE) $(RUN_ARGS)

docker-stop:
	docker stop $(DOCKER_STOP_ARGS) $(PROJECT)

docker-join:
	docker exec \
		-it $(PROJECT) \
		erl \
			-start_epmd false \
			-remsh $(PROJECT)@localhost \
			-sname $(PROJECT)-$$RANDOM \
			-setcookie $(PROJECT)

docker-shell:
	docker exec -it $(PROJECT) sh

docker-attach:
	docker attach $(PROJECT)

docker-logs:
	docker logs $(PROJECT)

docker-logs-f:
	docker logs -f $(PROJECT)

docker-clean:
	docker system prune -f --filter label=project=$(PROJECT)

docker-distclean: docker-clean
	docker rmi $(IMAGE_LATEST) $(IMAGE) 2>/dev/null || true

docker-help: docker-usage
docker-usage:
	@echo "Usage: make <Target> [Variables]"
	@echo
	@echo "Targets"
	$(usage-docker-targets)
	@echo
	@echo "Variables"
	$(usage-docker-variables)

define usage-docker-targets
	@printf \
		'$(shell printf "    %%-$(DOCKER_USAGE_PADDING)s %%s\\\n%.0s" {1..14})'\
	docker-build "Build image \"$(IMAGE)\"" \
	docker-push "Push image \"$(IMAGE)\"" \
	docker-local-release "Tag \"$(IMAGE)\" as \"$(IMAGE_LATEST)\"" \
	docker-release "Same as \"docker-local-release\" and push both images" \
	docker-run "Run \"$(IMAGE)\" as container \"$(PROJECT)\""  \
	docker-run-d "Same as \"docker-run\" but run in background" \
	docker-stop "Stop and remove running container \"$(PROJECT)\"" \
	docker-join \
		"Remsh to Erlang application in running container \"$(PROJECT)\"" \
	docker-shell "Exec shell in running container \"$(PROJECT)\"" \
	docker-attach "Attach to running container \"$(PROJECT)\"" \
	docker-logs "Print running \"$(PROJECT)\" container logs" \
	docker-logs-f "Same as \"docker-logs\" but follow log output" \
	docker-clean "Prune everything with label \"project=$(PROJECT)\"" \
	docker-distclean "Remove images \"$(IMAGE)\" and \"$(IMAGE_LATEST)\""
endef

define usage-docker-variables
	@printf \
		'$(shell printf "    %%-$(DOCKER_USAGE_PADDING)s %%s\\\n%.0s" {1..9})' \
	REGISTRY "Docker registry (current: \"$(REGISTRY)\")" \
	USER "Used as Docker ID (current: \"$(USER)\")" \
	PROJECT \
		"Used as image and running container name (current: \"$(PROJECT)\")" \
	VERSION "Version (current: \"$(VERSION)\")" \
	RUN_ARGS "Container entrypoint arguments (current: \"$(RUN_ARGS)\")" \
	DOCKER_RUN_ARGS \
		"Container running arguments (current: \"$(DOCKER_RUN_ARGS)\")" \
	DOCKER_RUN_ARGS_EXTRA \
		"Appends to DOCKER_RUN_ARGS (current: \"$(DOCKER_RUN_ARGS_EXTRA)\")" \
	DOCKER_BUILD_ARGS \
		"Image building arguments (current: \"$(DOCKER_BUILD_ARGS)\")" \
	DOCKER_BUILD_ARGS_EXTRA \
		"Prepends to DOCKER_BUILD_ARGS (current: \"$(DOCKER_BUILD_ARGS_EXTRA)\")"
endef
