IMAGE="ghcr.io/cdcgov/$1"
TAG=$2
BUILDER=docker-container-driver-builder

# create a builder with the docker-container driver to allow cache-export
docker buildx create --name "$BUILDER" --driver=docker-container || true

# use the registry cache for prior images of the same tag, or the 'latest' tag
time docker buildx build --push -t "$IMAGE:$TAG" \
	--builder "$BUILDER" \
	--cache-from "type=registry,ref=$IMAGE:$TAG-cache" \
	--cache-from "type=registry,ref=$IMAGE:latest-cache" \
	--cache-to "type=registry,ref=$IMAGE:$TAG-cache,mode=max" \
	-f Dockerfile .
