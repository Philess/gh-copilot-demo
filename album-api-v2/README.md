# album-api-v2

Node.js/TypeScript rewrite of the .NET `albums-api`. Manages music albums in memory with no database required.

## Requirements

- Node.js 18+
- npm 9+

## Install

```bash
npm install
```

## Development

Runs the server with hot-reload via `tsx`:

```bash
npm run dev
```

## Build

Compiles TypeScript to `dist/`:

```bash
npm run build
```

## Start

Starts the compiled build on port 3000:

```bash
npm start
```

The API is available at `http://localhost:3000`.

## Tests

```bash
npm test
```

Runs 24 Vitest tests covering all routes, happy paths, and error cases.

## API Routes

All routes are prefixed with `/albums`.

| Method | Route | Description | Success | Error |
|--------|-------|-------------|---------|-------|
| GET | `/albums` | List all albums | 200 | — |
| GET | `/albums/:id` | Get album by ID | 200 | 404 |
| POST | `/albums` | Create a new album | 201 | — |
| PUT | `/albums/:id` | Update an album | 200 | 404 |
| DELETE | `/albums/:id` | Delete an album | 204 | 404 |
| GET | `/albums/sort?sortBy=` | Sort albums by `title`, `artist`, or `price` | 200 | 400 |
| GET | `/albums/search?year=` | Filter albums by release year | 200 | — |

### Album shape

```json
{
  "id": 1,
  "title": "You, Me and an App Id",
  "artist": {
    "name": "Daprize",
    "birthdate": "1992-05-14",
    "birthPlace": "Seattle"
  },
  "year": 2020,
  "price": 10.99,
  "image_url": "https://aka.ms/albums-daprlogo"
}
```

### POST / PUT request body

```json
{
  "title": "My Album",
  "artist": {
    "name": "My Artist",
    "birthdate": "1990-01-01",
    "birthPlace": "London"
  },
  "year": 2024,
  "price": 12.99,
  "image_url": "https://example.com/cover.jpg"
}
```

## Vue App Compatibility

The API starts on port 3000, matching the Vite proxy configured in `album-viewer/vite.config.ts`. No changes to the frontend are required.
