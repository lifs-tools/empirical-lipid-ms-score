#!/bin/bash

docker buildx build --platform linux/amd64 -t docker.lifs-tools.org/lifs/eposmol .
