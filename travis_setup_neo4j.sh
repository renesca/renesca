#!/bin/bash -e
mkdir -p $HOME/neo4j
cd $HOME/neo4j

blocking-start() {
    neo4j-community-$NEO4J_VERSION/bin/neo4j start
    case $NEO4J_VERSION in
        3.*)
            # wait for the server to start
            fifo=/tmp/tmpfifo.$$
            mkfifo "${fifo}" || exit 1
            tail -f neo4j-community-$NEO4J_VERSION/logs/neo4j.log | tee ${fifo} &
            tailpid=$! # optional
            grep -m 1 "Started." "${fifo}"
            kill "${tailpid}" # optional
            rm "${fifo}"
            ;;
        2.*)
            # start script is blocking itself, so nothing to do here
            ;;
    esac
}

change-password() {
    sleep 20
    echo "changing password..."
    curl -H 'Content-Type:application/json' -H 'Authorization:Basic bmVvNGo6bmVvNGo=' -d '{"password":"testingpw"}' http://localhost:7474/user/neo4j/password
}

if [ ! -d neo4j-community-$NEO4J_VERSION ]; then
    wget dist.neo4j.org/neo4j-community-$NEO4J_VERSION-unix.tar.gz
    tar -xzf neo4j-community-$NEO4J_VERSION-unix.tar.gz
    case $NEO4J_VERSION in
        3.*)
            # sed -i.bak s/\#dbms.security.auth_enabled=false/dbms.security.auth_enabled=false/g neo4j-community-$NEO4J_VERSION/conf/neo4j.conf
            echo "org.neo4j.server.transaction.timeout=6" >> neo4j-community-$NEO4J_VERSION/conf/neo4j.conf
            echo "dbms.memory.pagecache.size=1G" >> neo4j-community-$NEO4J_VERSION/conf/neo4j.conf
            echo "dbms.memory.heap.max_size=1000" >> neo4j-community-$NEO4J_VERSION/conf/neo4j-wrapper.conf
            echo "dbms.memory.heap.initial_size=1000" >> neo4j-community-$NEO4J_VERSION/conf/neo4j-wrapper.conf
            ;;
        2.*)
            echo "org.neo4j.server.transaction.timeout=6" >> neo4j-community-$NEO4J_VERSION/conf/neo4j-server.properties
            ;;
    esac
    blocking-start
    change-password
else
    blocking-start
fi

