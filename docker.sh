#!/usr/bin/env bash

# double-slash because of:
# https://stackoverflow.com/questions/50608301/docker-mounted-volume-adds-c-to-end-of-windows-path-when-translating-from-linux
# https://stackoverflow.com/questions/53014998/msys2-and-docker-run-specifying-the-command-looks-for-the-command-locally-befor
docker build -t haskell-interpreter-docker - < .devcontainer/Dockerfile
docker run --rm -it -v /$(pwd)://workspace -w=//workspace haskell-interpreter-docker bash
