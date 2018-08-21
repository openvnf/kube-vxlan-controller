IMAGE = $(USER)/$(PROJECT):$(VERSION)
IMAGE_LATEST = $(USER)/$(PROJECT):latest

ifndef DOCKER_RUN_ARGS
	DOCKER_RUN_ARGS = -it --rm --name $(PROJECT) $(DOCKER_RUN_ARGS_EXTRA)
endif

docker-build:
	docker build . -t $(IMAGE)

docker-push:
	docker push $(IMAGE)

docker-release: docker-release-local
	docker push $(IMAGE_LATEST)

docker-local-release:
	docker tag $(IMAGE) $(IMAGE_LATEST)

docker-run:
	docker run $(DOCKER_RUN_ARGS) $(IMAGE) run

docker-run-d:
	docker run -d $(DOCKER_RUN_ARGS) $(IMAGE) run

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
	docker rmi $(IMAGE) 2>/dev/null || true

docker-distclean: docker-clean
	docker rmi $(IMAGE_LATEST) 2>/dev/null || true
