import { describe, it, expect, beforeEach } from 'vitest'
import request from 'supertest'
import app from '../app'
import { resetStore } from '../data/store'

beforeEach(() => {
  resetStore()
})

// ---------------------------------------------------------------------------
// GET /albums
// ---------------------------------------------------------------------------
describe('GET /albums', () => {
  it('returns 200 with all 6 seed albums', async () => {
    const res = await request(app).get('/albums')
    expect(res.status).toBe(200)
    expect(res.body).toHaveLength(6)
  })

  it('returns albums with expected shape', async () => {
    const res = await request(app).get('/albums')
    const album = res.body[0]
    expect(album).toHaveProperty('id')
    expect(album).toHaveProperty('title')
    expect(album).toHaveProperty('artist')
    expect(album.artist).toHaveProperty('name')
    expect(album.artist).toHaveProperty('birthdate')
    expect(album.artist).toHaveProperty('birthPlace')
    expect(album).toHaveProperty('year')
    expect(album).toHaveProperty('price')
    expect(album).toHaveProperty('image_url')
  })
})

// ---------------------------------------------------------------------------
// GET /albums/:id
// ---------------------------------------------------------------------------
describe('GET /albums/:id', () => {
  it('returns 200 for an existing id', async () => {
    const res = await request(app).get('/albums/1')
    expect(res.status).toBe(200)
    expect(res.body.id).toBe(1)
    expect(res.body.title).toBe('You, Me and an App Id')
  })

  it('returns 404 for an unknown id', async () => {
    const res = await request(app).get('/albums/999')
    expect(res.status).toBe(404)
  })
})

// ---------------------------------------------------------------------------
// GET /albums/search
// ---------------------------------------------------------------------------
describe('GET /albums/search', () => {
  it('returns only albums matching the given year', async () => {
    const res = await request(app).get('/albums/search?year=2021')
    expect(res.status).toBe(200)
    expect(res.body).toHaveLength(2)
    expect(res.body.every((a: { year: number }) => a.year === 2021)).toBe(true)
  })

  it('returns empty array when no albums match the year', async () => {
    const res = await request(app).get('/albums/search?year=1900')
    expect(res.status).toBe(200)
    expect(res.body).toHaveLength(0)
  })
})

// ---------------------------------------------------------------------------
// GET /albums/sort
// ---------------------------------------------------------------------------
describe('GET /albums/sort', () => {
  it('returns 400 when sortBy is missing', async () => {
    const res = await request(app).get('/albums/sort')
    expect(res.status).toBe(400)
  })

  it('returns 400 for an invalid sortBy value', async () => {
    const res = await request(app).get('/albums/sort?sortBy=invalid')
    expect(res.status).toBe(400)
  })

  it('returns 200 sorted alphabetically by title', async () => {
    const res = await request(app).get('/albums/sort?sortBy=title')
    expect(res.status).toBe(200)
    const titles: string[] = res.body.map((a: { title: string }) => a.title)
    expect(titles).toEqual([...titles].sort((a, b) => a.localeCompare(b)))
  })

  it('returns 200 sorted alphabetically by artist name', async () => {
    const res = await request(app).get('/albums/sort?sortBy=artist')
    expect(res.status).toBe(200)
    const names: string[] = res.body.map((a: { artist: { name: string } }) => a.artist.name)
    expect(names).toEqual([...names].sort((a, b) => a.localeCompare(b)))
  })

  it('returns 200 sorted ascending by price', async () => {
    const res = await request(app).get('/albums/sort?sortBy=price')
    expect(res.status).toBe(200)
    const prices: number[] = res.body.map((a: { price: number }) => a.price)
    for (let i = 1; i < prices.length; i++) {
      expect(prices[i]).toBeGreaterThanOrEqual(prices[i - 1])
    }
  })

  it('is case-insensitive for sortBy parameter', async () => {
    const res = await request(app).get('/albums/sort?sortBy=TITLE')
    expect(res.status).toBe(200)
    expect(res.body).toHaveLength(6)
  })
})

// ---------------------------------------------------------------------------
// POST /albums
// ---------------------------------------------------------------------------
describe('POST /albums', () => {
  const newAlbum = {
    title: 'Test Album',
    artist: { name: 'Test Artist', birthdate: '2000-01-01', birthPlace: 'London' },
    year: 2023,
    price: 9.99,
    image_url: 'https://example.com/test.jpg',
  }

  it('returns 201 with the new album assigned id 7', async () => {
    const res = await request(app).post('/albums').send(newAlbum)
    expect(res.status).toBe(201)
    expect(res.body.id).toBe(7)
  })

  it('persists all artist details correctly', async () => {
    const res = await request(app)
      .post('/albums')
      .send({ ...newAlbum, artist: { name: 'My Artist', birthdate: '1995-06-15', birthPlace: 'Tokyo' } })
    expect(res.body.artist.name).toBe('My Artist')
    expect(res.body.artist.birthdate).toBe('1995-06-15')
    expect(res.body.artist.birthPlace).toBe('Tokyo')
  })

  it('new album is retrievable via GET /albums/:id after creation', async () => {
    await request(app).post('/albums').send(newAlbum)
    const res = await request(app).get('/albums/7')
    expect(res.status).toBe(200)
    expect(res.body.title).toBe('Test Album')
  })

  it('increments total album count to 7', async () => {
    await request(app).post('/albums').send(newAlbum)
    const res = await request(app).get('/albums')
    expect(res.body).toHaveLength(7)
  })
})

// ---------------------------------------------------------------------------
// PUT /albums/:id
// ---------------------------------------------------------------------------
describe('PUT /albums/:id', () => {
  const update = {
    title: 'Updated Title',
    artist: { name: 'Updated Artist', birthdate: '1990-01-01', birthPlace: 'NYC' },
    year: 2023,
    price: 19.99,
    image_url: 'https://example.com/updated.jpg',
  }

  it('returns 200 with the updated album for an existing id', async () => {
    const res = await request(app).put('/albums/1').send(update)
    expect(res.status).toBe(200)
    expect(res.body.id).toBe(1)
    expect(res.body.title).toBe('Updated Title')
    expect(res.body.price).toBe(19.99)
  })

  it('persists the update so subsequent GET returns new values', async () => {
    await request(app).put('/albums/1').send(update)
    const res = await request(app).get('/albums/1')
    expect(res.body.title).toBe('Updated Title')
  })

  it('returns 404 for an unknown id', async () => {
    const res = await request(app).put('/albums/999').send(update)
    expect(res.status).toBe(404)
  })
})

// ---------------------------------------------------------------------------
// DELETE /albums/:id
// ---------------------------------------------------------------------------
describe('DELETE /albums/:id', () => {
  it('returns 204 for an existing id', async () => {
    const res = await request(app).delete('/albums/1')
    expect(res.status).toBe(204)
  })

  it('removes the album so a subsequent GET returns 404', async () => {
    await request(app).delete('/albums/1')
    const res = await request(app).get('/albums/1')
    expect(res.status).toBe(404)
  })

  it('reduces total album count by 1', async () => {
    await request(app).delete('/albums/1')
    const res = await request(app).get('/albums')
    expect(res.body).toHaveLength(5)
  })

  it('returns 404 for an unknown id', async () => {
    const res = await request(app).delete('/albums/999')
    expect(res.status).toBe(404)
  })
})

// ---------------------------------------------------------------------------
// Root endpoint
// ---------------------------------------------------------------------------
describe('GET /', () => {
  it('returns a friendly hint message', async () => {
    const res = await request(app).get('/')
    expect(res.status).toBe(200)
    expect(res.text).toContain('/albums')
  })
})
