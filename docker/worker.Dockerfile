FROM debian:buster-slim

RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates

RUN update-ca-certificates

COPY ./worker-exe /worker-exe
RUN chmod +x /worker-exe
CMD ["/worker-exe"]
