# album-api-v2 (Node + TypeScript)

A Node.js + TypeScript rewrite of the existing .NET `albums-api`. It serves the same album data and endpoints expected by the Vue app, with in-memory storage and no database.

## Prerequisites
- Node.js 18+ (installed in this dev container)

## Install, Build, Test, Run

```bash
# From the repo root or this folder
cd /workspaces/gh-copilot-demo/album-api-v2

# Install dependencies
npm install

# Build TypeScript
npm run build

# Run tests
npm test

# Start the server (port 3000)
npm start

# Optional: Developer mode with auto-restart
npm run dev
```

## Server
- Listens on `http://localhost:3000`.
- CORS enabled.
- In-memory album collection seeded with the exact same sample data as the original .NET `albums-api`.

## Routes
- `GET /` — returns text: "Hit the /albums endpoint to retrieve a list of albums!"
- `GET /albums` — returns the full album list (6 items).
- `GET /albums/:id` — returns a single album by id; `404` if not found.
- `POST /albums` — creates an album.
  - Payload: `{ title: string, artist: string, price: number, image_url: string }`
  - Returns: created album with auto-incremented `id`.
- `PUT /albums/:id` — updates an existing album.
  - Payload: same as `POST`.
  - Returns: updated album; `404` if not found.
- `DELETE /albums/:id` — deletes an album; returns `204`.

## Data Model
```ts
export interface Album {
  id: number;
  title: string;
  artist: string;
  price: number;
  image_url: string;
}
```

## Sample Data (seeded)
```
[
  { "id": 1, "title": "You, Me and an App Id", "artist": "Daprize", "price": 10.99, "image_url": "https://aka.ms/albums-daprlogo" },
  { "id": 2, "title": "Seven Revision Army", "artist": "The Blue-Green Stripes", "price": 13.99, "image_url": "https://aka.ms/albums-containerappslogo" },
  { "id": 3, "title": "Scale It Up", "artist": "KEDA Club", "price": 13.99, "image_url": "https://aka.ms/albums-kedalogo" },
  { "id": 4, "title": "Lost in Translation", "artist": "MegaDNS", "price": 12.99, "image_url": "https://aka.ms/albums-envoylogo" },
  { "id": 5, "title": "Lock Down Your Love", "artist": "V is for VNET", "price": 12.99, "image_url": "https://aka.ms/albums-vnetlogo" },
  { "id": 6, "title": "Sweet Container O' Mine", "artist": "Guns N Probeses", "price": 14.99, "image_url": "https://aka.ms/albums-containerappslogo" }
]
```

## Notes
- Matches the Vue app’s existing calls (proxy to `http://localhost:3000`).
- No database; all changes affect only runtime memory.
