IMAGE = $(USER)/$(PROJECT):$(VERSION)
IMAGE_LATEST = $(USER)/$(PROJECT):latest

docker-build:
	docker build . -t $(IMAGE)

docker-push:
	docker push $(IMAGE)

docker-release: docker-release-local
	docker push $(IMAGE_LATEST)

docker-local-release:
	docker tag $(IMAGE) $(IMAGE_LATEST)

docker-run:
	docker run \
		--rm -it \
		--name $(PROJECT) \
		-v ${PWD}/priv:/etc/$(PROJECT) \
		$(IMAGE) run

docker-start:
	docker run \
		--rm -itd \
		--name $(PROJECT) \
		-v ${PWD}/priv:/etc/$(PROJECT) \
		$(IMAGE) run

docker-stop:
	docker stop $(PROJECT)

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
