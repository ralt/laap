push: build
	docker push ralt/laap:latest

build: Dockerfile
	docker build -t ralt/laap:latest .
	@touch $@
