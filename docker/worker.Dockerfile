FROM debian:buster-slim
COPY ./worker-exe /worker-exe
RUN chmod +x /worker-exe
CMD ["/worker-exe"]
