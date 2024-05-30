FROM haskell:9.4.8-slim-buster
WORKDIR /app
CMD ["stack", "run"]
