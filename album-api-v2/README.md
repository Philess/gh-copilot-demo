# Album API v2

Node.js/TypeScript API for managing music albums. This is a rewrite of the previous .NET `albums-api` in TypeScript.

## Features

- ✅ List all albums
- ✅ Get album by ID
- ✅ Create new album
- ✅ Update existing album
- ✅ Delete album
- ✅ In-memory data storage (no database required)
- ✅ CORS enabled
- ✅ Full TypeScript support
- ✅ Comprehensive unit tests

## Sample Data

The API comes pre-loaded with 6 sample albums:
1. "You, Me and an App Id" by Daprize - $10.99
2. "Seven Revision Army" by The Blue-Green Stripes - $13.99
3. "Scale It Up" by KEDA Club - $13.99
4. "Lost in Translation" by MegaDNS - $12.99
5. "Lock Down Your Love" by V is for VNET - $12.99
6. "Sweet Container O' Mine" by Guns N Probeses - $14.99

## API Endpoints

### Get all albums
```
GET /albums
```

### Get album by ID
```
GET /albums/:id
```

### Create a new album
```
POST /albums
Content-Type: application/json

{
  "title": "Album Title",
  "artist": "Artist Name",
  "price": 15.99,
  "image_url": "https://example.com/image.jpg"
}
```

### Update an album
```
PUT /albums/:id
Content-Type: application/json

{
  "title": "Updated Title",
  "price": 20.99
}
```

### Delete an album
```
DELETE /albums/:id
```

## Getting Started

### Prerequisites

- Node.js (v18 or higher recommended)
- npm or yarn

### Installation

1. Navigate to the project directory:
```bash
cd album-api-v2
```

2. Install dependencies:
```bash
npm install
```

### Development Mode

Run the API in development mode with hot reload:
```bash
npm run dev
```

The server will start on `http://localhost:3000`

### Build for Production

Compile TypeScript to JavaScript:
```bash
npm run build
```

### Start Production Server

After building, start the server:
```bash
npm start
```

The server runs on port **3000** by default (configurable via `PORT` environment variable).

## Testing

### Run All Tests

```bash
npm test
```

### Run Tests in Watch Mode

```bash
npm run test:watch
```

### Run Tests with Coverage

```bash
npm run test:coverage
```

Test coverage includes:
- ✅ 15 unit tests
- ✅ All API endpoints tested
- ✅ Success and error scenarios
- ✅ Input validation tests

## Project Structure

```
album-api-v2/
├── src/
│   ├── index.ts              # Main application entry point
│   ├── models/
│   │   └── album.ts          # Album interface
│   ├── data/
│   │   └── albumData.ts      # In-memory data storage and operations
│   └── routes/
│       ├── albums.ts         # Album routes and handlers
│       └── albums.test.ts    # Unit tests
├── dist/                     # Compiled JavaScript (after build)
├── package.json
├── tsconfig.json
├── jest.config.js
└── README.md
```

## API Response Examples

### Get All Albums
```json
[
  {
    "id": 1,
    "title": "You, Me and an App Id",
    "artist": "Daprize",
    "price": 10.99,
    "image_url": "https://aka.ms/albums-daprlogo"
  },
  ...
]
```

### Error Response
```json
{
  "error": "Album not found"
}
```

## Compatibility

This API is fully compatible with the existing Vue.js `album-viewer` application. The routes match the expected endpoints:
- `/albums` for listing albums
- CORS enabled for frontend access
- Same data structure as the original .NET API

## Notes

- Data is stored in memory and resets when the server restarts
- Port 3000 is configured to work seamlessly with the Vue.js frontend
- All endpoints return JSON responses
- Proper HTTP status codes are used (200, 201, 204, 400, 404)
