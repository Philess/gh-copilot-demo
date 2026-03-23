import request from 'supertest'
import { beforeEach, describe, expect, it } from 'vitest'

import { createApp } from '../src/app.js'
import { sampleAlbums } from '../src/data/sampleAlbums.js'
import { AlbumsStore } from '../src/store/albumsStore.js'
import type { AlbumRequest } from '../src/types.js'

describe('album-api-v2', () => {
  const store = new AlbumsStore()
  const app = createApp(store)

  beforeEach(() => {
    store.reset()
  })

  it('returns the exact seeded albums collection', async () => {
    const response = await request(app).get('/albums')

    expect(response.status).toBe(200)
    expect(response.body).toEqual(sampleAlbums)
  })

  it('returns one album by id', async () => {
    const response = await request(app).get('/albums/2')

    expect(response.status).toBe(200)
    expect(response.body).toEqual(sampleAlbums[1])
  })

  it('returns 404 when an album does not exist', async () => {
    const response = await request(app).get('/albums/999')

    expect(response.status).toBe(404)
  })

  it('creates a new album with the next id', async () => {
    const newAlbum: AlbumRequest = {
      title: 'The Event Loop Mixtape',
      artist: {
        name: 'Async Anonymous',
        birthdate: '1991-04-12',
        birthPlace: 'Berlin, Germany',
      },
      year: 2024,
      price: 15.99,
      image_url: 'https://aka.ms/albums-daprlogo',
    }

    const response = await request(app).post('/albums').send(newAlbum)

    expect(response.status).toBe(201)
    expect(response.headers.location).toBe('/albums/7')
    expect(response.body).toEqual({
      id: 7,
      ...newAlbum,
    })
    expect(store.getById(7)).toEqual({
      id: 7,
      ...newAlbum,
    })
  })

  it('updates an existing album', async () => {
    const update: AlbumRequest = {
      title: 'Updated Album',
      artist: {
        name: 'Updated Artist',
        birthdate: '1980-10-10',
        birthPlace: 'Paris, France',
      },
      year: 2025,
      price: 17.5,
      image_url: 'https://aka.ms/albums-envoylogo',
    }

    const response = await request(app).put('/albums/3').send(update)

    expect(response.status).toBe(200)
    expect(response.body).toEqual({
      id: 3,
      ...update,
    })
    expect(store.getById(3)).toEqual({
      id: 3,
      ...update,
    })
  })

  it('returns 404 when updating a missing album', async () => {
    const response = await request(app).put('/albums/404').send(sampleAlbums[0])

    expect(response.status).toBe(404)
  })

  it('deletes an existing album', async () => {
    const response = await request(app).delete('/albums/4')

    expect(response.status).toBe(204)
    expect(store.getById(4)).toBeUndefined()
  })

  it('returns 404 when deleting a missing album', async () => {
    const response = await request(app).delete('/albums/404')

    expect(response.status).toBe(404)
  })

  it('reuses max id plus one after deletions', async () => {
    await request(app).delete('/albums/6')

    const newAlbum: AlbumRequest = {
      title: 'After Delete',
      artist: {
        name: 'Still Sequential',
        birthdate: '1999-09-09',
        birthPlace: 'Madrid, Spain',
      },
      year: 2026,
      price: 18.25,
      image_url: 'https://aka.ms/albums-kedalogo',
    }

    const response = await request(app).post('/albums').send(newAlbum)

    expect(response.status).toBe(201)
    expect(response.body.id).toBe(6)
  })
})