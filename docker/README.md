# Docker Images

*** All commands below are to be run inside this dir***

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

### Worker container

The container containing the application to run misc tasks

Build the application first using the development container

```
kl exec -it deployment/development-container -- cabal install application:worker-exe --installdir=./dist --install-method=copy --overwrite-policy=always
```

Then copy the ugly file over and load it into the new image

```
cp ../application/dist/worker-exe ./worker-exe
docker buildx build -f ./worker.Dockerfile -t 20544dk/worker:1.0 --platform linux/arm64  --push .
rm ./worker-exe
```
