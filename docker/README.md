# Docker Images

### Docker Login

Login to docker hub via

```
echo $DOCKER_HUB_TOKEN | docker login --username $DOCKER_HUB_USERNAME --password-stdin
```

### Dev Container

Just the standard haskell container with some customisations

To build and push:

```
docker buildx build -f ./dev-container.Dockerfile -t 20544dk/dev-container:1.0 --platform linux/arm64  --push .
```
