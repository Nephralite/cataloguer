FROM rust:1.93-slim-buster
WORKDIR /usr/src/myapp
COPY . .
RUN cargo build --release
CMD ["./target/release/cataloguer"]
