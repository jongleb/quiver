# The release binaries in a minimal image: glibc, CA roots and a non-root user.
# The build context holds linux-amd64/quiver and linux-arm64/quiver.
FROM gcr.io/distroless/base-debian12:nonroot
ARG TARGETARCH
COPY --chmod=0755 linux-${TARGETARCH}/quiver /usr/local/bin/quiver
WORKDIR /work
ENTRYPOINT ["/usr/local/bin/quiver"]
