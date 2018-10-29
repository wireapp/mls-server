##
## Production
##

# Build the project
.PHONY: build
build:
	stack build --test --no-run-tests

# Initialize the "mls" keyspace and/or perform migrations
.PHONY: schema
schema:
	stack exec mls-server-schema

# Run the project using production settings (with Cassandra)
.PHONY: run
run:
	stack exec mls-server

# Run the project using in-memory storage
.PHONY: run-in-memory
run-in-memory:
	CONFIG_FILE=./conf/test/mls-server.yaml stack exec mls-server

##
## Testing
##

# Test the project with in-memory storage
.PHONY: test
test:
	stack test

# Run the project with Cassandra storage:
#
#   * Cassandra has to be running on port 9042
#   * A schema has to be created with "make schema-test"
.PHONY: test-cassandra
test-cassandra:
	CONFIG_FILE=./conf/test/mls-server-cassandra.yaml stack test

# Initialize the "mls_test" keyspace and/or perform migrations
.PHONY: schema-test
schema-test:
	CONFIG_FILE=./conf/test/mls-server-schema.yaml stack exec mls-server-schema
