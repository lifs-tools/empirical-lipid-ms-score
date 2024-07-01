#!/bin/bash
# start the eposmol Docker stack and rebuild if any source file changes
docker compose -f docker-compose-eposmol.yml up --watch --build
