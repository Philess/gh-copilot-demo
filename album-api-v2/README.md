# album-api-v2

Node.js + TypeScript rewrite of the original `albums-api` service.

This API manages a collection of music albums entirely in memory and preserves the existing backend payload shape, including the nested `artist` object.

## Requirements

- Node.js 18 or later
- npm

## Install

```bash
npm install
```

## Run In Development

Starts the API with file watching.

```bash
npm run dev
```

The API listens on port `3000` by default.

## Build

Compiles the TypeScript source into `dist/`.

```bash
npm run build
```

## Run The Built Application

```bash
npm start
```

The production start command runs the compiled server from `dist/src/index.js`.

## Run Tests

```bash
npm test
```

The test suite covers:

- listing all albums
- getting one album by id
- creating albums
- updating albums
- deleting albums
- 404 behavior for missing albums
- seed data parity with the original .NET API

## API Base URL

```text
http://localhost:3000
```

## Routes

### `GET /`

Returns a plain-text help message.

### `GET /albums`

Returns the full in-memory album collection.

### `GET /albums/:id`

Returns a single album by id.

- `200 OK` when found
- `404 Not Found` when missing

### `POST /albums`

Creates a new album.

- `201 Created` when successful
- Returns the created album
- Sets the `Location` header to `/albums/{id}`

### `PUT /albums/:id`

Replaces an existing album.

- `200 OK` when found
- `404 Not Found` when missing

### `DELETE /albums/:id`

Deletes an album.

- `204 No Content` when deleted
- `404 Not Found` when missing

## Album Payload Shape

```json
{
  "id": 1,
  "title": "You, Me and an App Id",
  "artist": {
    "name": "Daprize",
    "birthdate": "1988-05-14",
    "birthPlace": "Seattle, USA"
  },
  "year": 2020,
  "price": 10.99,
  "image_url": "https://aka.ms/albums-daprlogo"
}
```

For `POST` and `PUT`, send the same payload without the `id` field.

## Notes

- Data is stored in memory only.
- Restarting the process resets the collection to the seeded sample data.
- CORS is enabled.
- The API is configured to match the existing Vue app route path of `/albums` on port `3000`.