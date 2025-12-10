# Album API v2

A Node.js/TypeScript REST API for managing music albums. This is a rewrite of the original .NET `albums-api` with full CRUD operations.

## Features

- ✅ Full CRUD operations (Create, Read, Update, Delete)
- ✅ In-memory data storage
- ✅ TypeScript for type safety
- ✅ Express.js web framework
- ✅ CORS enabled for all origins
- ✅ Comprehensive unit tests with Jest
- ✅ Compatible with existing Vue.js frontend

## Project Structure

```
album-api-v2/
├── src/
│   ├── server.ts              # Express server setup
│   ├── models/
│   │   └── Album.ts           # Album interface
│   ├── data/
│   │   └── albumStore.ts      # In-memory data store
│   ├── routes/
│   │   └── albums.ts          # Album CRUD endpoints
│   └── __tests__/
│       └── albums.test.ts     # Unit tests
├── dist/                      # Compiled JavaScript (generated)
├── package.json
├── tsconfig.json
└── jest.config.js
```

## Installation

```bash
npm install
```

## Usage

### Development Mode (with hot reload)
```bash
npm run dev
```

### Production Mode
```bash
# Build the application
npm run build

# Start the server
npm start
```

The server will start on **http://localhost:3000**

## API Endpoints

| Method | Endpoint | Description | Request Body | Response |
|--------|----------|-------------|--------------|----------|
| GET | `/` | Welcome message | - | Text message |
| GET | `/albums` | List all albums | - | Array of albums |
| GET | `/albums/:id` | Get single album | - | Album object |
| POST | `/albums` | Create new album | Album data (without id) | Created album (201) |
| PUT | `/albums/:id` | Update album | Partial album data | Updated album |
| DELETE | `/albums/:id` | Delete album | - | No content (204) |

### Album Model

```typescript
{
  id: number;           // Auto-generated
  title: string;        // Album title
  artist: string;       // Artist name
  price: number;        // Price in USD
  image_url: string;    // Album cover image URL
}
```

## Example Requests

### Get all albums
```bash
curl http://localhost:3000/albums
```

### Get single album
```bash
curl http://localhost:3000/albums/1
```

### Create new album
```bash
curl -X POST http://localhost:3000/albums \
  -H "Content-Type: application/json" \
  -d '{
    "title": "New Album",
    "artist": "Artist Name",
    "price": 15.99,
    "image_url": "https://example.com/image.jpg"
  }'
```

### Update album
```bash
curl -X PUT http://localhost:3000/albums/1 \
  -H "Content-Type: application/json" \
  -d '{"price": 12.99}'
```

### Delete album
```bash
curl -X DELETE http://localhost:3000/albums/1
```

## Sample Data

The API comes pre-loaded with 6 sample albums:

1. **You, Me and an App Id** by Daprize - $10.99
2. **Seven Revision Army** by The Blue-Green Stripes - $13.99
3. **Scale It Up** by KEDA Club - $13.99
4. **Lost in Translation** by MegaDNS - $12.99
5. **Lock Down Your Love** by V is for VNET - $12.99
6. **Sweet Container O' Mine** by Guns N Probeses - $14.99

## Testing

Run the test suite:

```bash
npm test
```

Run tests in watch mode:

```bash
npm run test:watch
```

### Test Coverage

The test suite includes:
- ✅ GET all albums - list functionality
- ✅ GET single album - by ID with validation
- ✅ POST new album - with validation for required fields and price
- ✅ PUT update album - full and partial updates
- ✅ DELETE album - removal and verification
- ✅ Integration test - complete CRUD workflow
- ✅ Error handling - 404s, 400s for invalid input

**Test Results: 19 tests, all passing ✓**

## Vue.js Integration

This API is fully compatible with the existing Vue.js frontend (`album-viewer`). The Vue app:
- Runs on port 3001
- Proxies `/albums` requests to `http://localhost:3000`
- Expects the same response format as the original .NET API

## Configuration

### Environment Variables

- `PORT` - Server port (default: 3000)

### CORS

CORS is enabled for all origins to allow the Vue.js frontend to connect.

## Development

Built with:
- **Node.js** - Runtime environment
- **TypeScript** - Type-safe JavaScript
- **Express.js** - Web framework
- **Jest** - Testing framework
- **Supertest** - HTTP testing library

## Notes

- Data is stored in-memory and will reset when the server restarts
- New albums are assigned auto-incrementing IDs starting from 7
- All endpoints include proper error handling and validation
- Price must be a positive number
- All fields are required when creating a new album
