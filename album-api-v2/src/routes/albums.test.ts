import { describe, it, expect } from 'vitest'
import request from 'supertest'
import app from '../app'

describe('GET /albums', () => {
  it('returns all seeded albums', async () => {
    const res = await request(app).get('/albums')
    expect(res.status).toBe(200)
    expect(res.body).toHaveLength(6)
    expect(res.body[0]).toMatchObject({ id: 1, title: 'You, Me and an App Id' })
  })
})

describe('GET /albums/:id', () => {
  it('returns 200 with the album for an existing id', async () => {
    const res = await request(app).get('/albums/2')
    expect(res.status).toBe(200)
    expect(res.body.title).toBe('Seven Revision Army')
  })

  it('returns 404 for a missing id', async () => {
    const res = await request(app).get('/albums/9999')
    expect(res.status).toBe(404)
  })

  it('returns 400 for a non-numeric id', async () => {
    const res = await request(app).get('/albums/not-a-number')
    expect(res.status).toBe(400)
  })
})

describe('GET /albums/year/:year', () => {
  it('returns albums matching the year', async () => {
    const res = await request(app).get('/albums/year/2019')
    expect(res.status).toBe(200)
    expect(res.body).toHaveLength(1)
    expect(res.body[0].title).toBe('Scale It Up')
  })

  it('returns an empty array for a year with no albums', async () => {
    const res = await request(app).get('/albums/year/1900')
    expect(res.status).toBe(200)
    expect(res.body).toEqual([])
  })
})

describe('GET /albums/sort', () => {
  it('sorts by title', async () => {
    const res = await request(app).get('/albums/sort').query({ sortBy: 'title' })
    expect(res.status).toBe(200)
    const titles = res.body.map((a: { title: string }) => a.title)
    expect(titles).toEqual([...titles].sort())
  })

  it('sorts by price', async () => {
    const res = await request(app).get('/albums/sort').query({ sortBy: 'price' })
    expect(res.status).toBe(200)
    const prices = res.body.map((a: { price: number }) => a.price)
    expect(prices).toEqual([...prices].sort((a, b) => a - b))
  })

  it('returns 400 for an invalid sortBy', async () => {
    const res = await request(app).get('/albums/sort').query({ sortBy: 'bogus' })
    expect(res.status).toBe(400)
  })
})

describe('POST /albums', () => {
  it('creates an album and returns 201 with a new id', async () => {
    const payload = {
      title: 'Integration Test Album',
      artist: { name: 'Test Artist', birthdate: '2000-01-01', birthPlace: 'Test City' },
      year: 2024,
      price: 5.99,
      image_url: 'https://example.com/test.png'
    }

    const res = await request(app).post('/albums').send(payload)
    expect(res.status).toBe(201)
    expect(res.body.id).toBeTypeOf('number')
    expect(res.body.title).toBe(payload.title)

    // cleanup
    await request(app).delete(`/albums/${res.body.id}`)
  })
})

describe('PUT /albums/:id', () => {
  it('updates an existing album and returns 200', async () => {
    const payload = {
      title: 'Lost in Translation (Remastered)',
      artist: { name: 'MegaDNS', birthdate: '1982-11-19', birthPlace: 'London' },
      year: 2018,
      price: 15.99,
      image_url: 'https://aka.ms/albums-envoylogo'
    }

    const res = await request(app).put('/albums/4').send(payload)
    expect(res.status).toBe(200)
    expect(res.body).toMatchObject({ id: 4, title: payload.title, price: 15.99 })

    // restore original
    await request(app).put('/albums/4').send({
      title: 'Lost in Translation',
      artist: { name: 'MegaDNS', birthdate: '1982-11-19', birthPlace: 'London' },
      year: 2018,
      price: 12.99,
      image_url: 'https://aka.ms/albums-envoylogo'
    })
  })

  it('returns 404 for a missing id', async () => {
    const res = await request(app).put('/albums/9999').send({
      title: 'Nope',
      artist: { name: 'Nobody', birthdate: '2000-01-01', birthPlace: 'Nowhere' },
      year: 2024,
      price: 1,
      image_url: 'https://example.com/nope.png'
    })
    expect(res.status).toBe(404)
  })
})

describe('DELETE /albums/:id', () => {
  it('deletes an existing album and returns 204', async () => {
    const created = await request(app).post('/albums').send({
      title: 'Delete Me',
      artist: { name: 'Temp Artist', birthdate: '2000-01-01', birthPlace: 'Nowhere' },
      year: 2024,
      price: 1,
      image_url: 'https://example.com/delete.png'
    })

    const res = await request(app).delete(`/albums/${created.body.id}`)
    expect(res.status).toBe(204)

    const getRes = await request(app).get(`/albums/${created.body.id}`)
    expect(getRes.status).toBe(404)
  })

  it('returns 404 for a missing id', async () => {
    const res = await request(app).delete('/albums/9999')
    expect(res.status).toBe(404)
  })
})
