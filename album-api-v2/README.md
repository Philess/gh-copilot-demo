# Album API v2

A Node.js TypeScript REST API for managing music albums. This is a rewrite of the .NET `albums-api` with the same functionality and data.

## Features

- 🎵 Full CRUD operations for music albums
- 📦 In-memory data storage (6 pre-loaded sample albums)
- 🔍 Search albums by release year
- ✅ Comprehensive unit tests with Jest and Supertest
- 🚀 TypeScript for type safety
- 🌐 CORS enabled for cross-origin requests

## Tech Stack

- **Runtime**: Node.js
- **Language**: TypeScript
- **Framework**: Express.js
- **Testing**: Jest + Supertest
- **Module System**: ES Modules

## API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/` | Welcome message |
| GET | `/albums` | Get all albums |
| GET | `/albums/:id` | Get album by ID |
| GET | `/albums/search?year={year}` | Search albums by year |
| POST | `/albums` | Create new album |
| PUT | `/albums/:id` | Update existing album |
| DELETE | `/albums/:id` | Delete album |

## Album Data Structure

```typescript
{
  id: number,
  title: string,
  artist: string,
  price: number,
  year: number,
  image_url: string
}
```

## Quick Start

### Install Dependencies

```bash
npm install
```

### Development Mode

```bash
npm run dev
```

Starts the server on port 3000 with hot-reload using tsx.

### Production Build

```bash
npm run build
npm start
```

### Run Tests

```bash
npm test
```

### Run Tests with Coverage

```bash
npm run test:coverage
```

## Sample Albums

The API comes pre-loaded with 6 sample albums:

1. **You, Me and an App Id** - Daprize (2021) - $10.99
2. **Seven Revision Army** - The Blue-Green Stripes (2020) - $13.99
3. **Scale It Up** - KEDA Club (2021) - $13.99
4. **Lost in Translation** - MegaDNS (2020) - $12.99
5. **Lock Down Your Love** - V is for VNET (2021) - $12.99
6. **Sweet Container O' Mine** - Guns N Probeses (2021) - $14.99

## Example Usage

### Get all albums
```bash
curl http://localhost:3000/albums
```

### Get album by ID
```bash
curl http://localhost:3000/albums/1
```

### Search albums by year
```bash
curl http://localhost:3000/albums/search?year=2021
```

### Create new album
```bash
curl -X POST http://localhost:3000/albums \
  -H "Content-Type: application/json" \
  -d '{
    "title": "New Album",
    "artist": "New Artist",
    "price": 15.99,
    "year": 2023,
    "image_url": "https://example.com/image.jpg"
  }'
```

### Update album
```bash
curl -X PUT http://localhost:3000/albums/1 \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Updated Album",
    "artist": "Updated Artist",
    "price": 19.99,
    "year": 2024,
    "image_url": "https://example.com/updated.jpg"
  }'
```

### Delete album
```bash
curl -X DELETE http://localhost:3000/albums/1
```

## Project Structure

```
album-api-v2/
├── src/
│   ├── __tests__/
│   │   ├── albumService.test.ts
│   │   └── albumRoutes.test.ts
│   ├── data/
│   │   └── albums.ts
│   ├── routes/
│   │   └── albumRoutes.ts
│   ├── services/
│   │   └── albumService.ts
│   ├── types/
│   │   └── album.ts
│   ├── app.ts
│   └── server.ts
├── dist/                    (compiled JavaScript)
├── package.json
├── tsconfig.json
├── jest.config.js
└── README.md
```

## Notes

- Data is stored in-memory and resets when the server restarts
- New album IDs start at 7 (after the 6 pre-loaded albums)
- The API matches the .NET `albums-api` functionality and endpoints
- Compatible with the Vue.js `album-viewer` app running on port 3001

## License

MIT
