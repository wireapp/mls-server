# mls-server

A prototype MLS server. See https://github.com/wireapp/melissa.

**Note: this project is currently not maintained and does not compile with Stack 2 or greater.
The problem is that it complains about symlinks in [wire-server](https://github.com/wireapp/wire-server), which this project depends on.**

## Running

First build the project: `make build`. Then:

### In-memory storage

  * `make run` to run the server
  * `make test` to test the server

### Cassandra storage

Ensure that Cassandra is running on port 9042.

  * `make schema run-db` to run the server
  * `make schema-test test-db` to test the server

## API

All endpoints accept and return JSON, unless stated otherwise.

### Schema

The server stores blobs. A `Blob` has the following schema:

```javascript
{
    "index"   : int >= 0,
    "content" : JSON
}
```

### `GET /groups/:id/blobs`

For a group, get a list of all blobs in some range, from oldest to newest.

**Parameters:**

| Name&nbsp;&nbsp;&nbsp; | Type | Description |
| --- | --- | --- |
| `:id` | Any&nbsp;string | Group ID. |
| `?from` | Int&nbsp;(optional) | The beginning of the range (inclusive). If this parameter is not specified, enumeration will start from the first blob. |
| `?to` | Int&nbsp;(optional) | The end of the range (exclusive). If this parameter is not specified, enumeration will proceed until the last blob. |

**Returns:**

| Status | Return&nbsp;type | Description |
| --- | --- | --- |
| 200 | Array&nbsp;of&nbsp;Blobs | An array with requested blobs. Can be empty. |
| 400 | - | The range is outside of the allowed range `[0; len)`, or the lower bound of the range is higher than the upper bound. |

**Error schema:**

```javascript
// The range is outside of the allowed range
{
    "tag": "BlobRangeOutOfBounds",
    "description": "...",
    "body":
        {
            "allowed_range": {"from": int, "to": int},
            "requested_range": {"from": int, "to": int}
        }
}
```

```javascript
// The lower bound of the range is higher than the upper bound
{
    "tag": "InvalidBlobRange",
    "description": "...",
    "body":
        {
            "requested_range": {"from": int, "to": int}
        }
}
```

### `POST /groups/:id/blobs`

Append a new blob to the list of blobs belonging to the group. If the group
didn't exist before, it will be created.

Blob indices have to go in order, starting from 0:

  * If the previous blob has index N, the one posted to this endpoint has to
    have index N+1.

  * If there are no blobs, the posted blob has to have index 0.

**Parameters:**

| Name | Type | Description |
| --- | --- | --- |
| `:id` | Any string | Group ID. |
| Body | Blob | The blob that should be appended to the list. |

**Returns:**

| Status | Return type | Description |
| --- | --- | --- |
| 204 | - | The blob has been appended. |
| 400 | - | The blob has the wrong index. |

**Error schema:**

```javascript
// The blob has the wrong index
{
    "tag": "UnexpectedBlobIndex",
    "description": "...",
    "body":
        {
            "expected_index": int,
            "got_index": int
        }
}
```

### `POST /i/reset`

Clear all storage. An internal endpoint.

**Returns:**

| Status | Return type | Description |
| --- | --- | --- |
| 204 | - | The storage has been cleared. |
