##
## Building
##

# Build the project
.PHONY: build
build:
	stack build --test --no-run-tests

##
## In-memory storage
##

# Run the project using in-memory storage
.PHONY: run
run:
	CONFIG_FILE=./conf/test/mls-server.yaml stack exec mls-server

# Test the project with in-memory storage
.PHONY: test
test:
	stack test

##
## Cassandra (has to be running on port 9042)
##

# Initialize the "mls" keyspace and/or perform migrations
.PHONY: schema
schema:
	stack exec mls-server-schema

# Initialize the "mls_test" keyspace and/or perform migrations
.PHONY: schema-test
schema-test:
	CONFIG_FILE=./conf/test/mls-server-schema.yaml stack exec mls-server-schema

# Run the project using production settings (with Cassandra)
.PHONY: run-db
run-db:
	stack exec mls-server

# Test the project with Cassandra
.PHONY: test-db
test-db:
	CONFIG_FILE=./conf/test/mls-server-cassandra.yaml stack test
