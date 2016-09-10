#!/bin/bash -e

docker run -p 7474:7474 -e NEO4J_AUTH="neo4j/testingpw" -d neo4j:$NEO4J_VERSION

while [[ -z "$(curl -u neo4j:testingpw localhost:7474 2> /dev/null)" ]]; do
    sleep 1
done
