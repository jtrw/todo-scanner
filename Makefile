$(shell (if [ ! -e .env ]; then cp .env.default .env; fi))
include .env
export

.PHONY docker-build:
docker-build:
	docker build -t $(DOCKER_IMAGE) .

.PHONE docker-push:
docker-push:
	docker push $(DOCKER_IMAGE)