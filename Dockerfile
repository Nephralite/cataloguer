FROM rust:1.78-slim-buster
WORKDIR /usr/src/myapp
COPY . .
RUN cargo build --release .
CMD ["./target/release/cataloguer"]
