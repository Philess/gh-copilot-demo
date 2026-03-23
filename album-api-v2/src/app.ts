import cors from 'cors'
import express from 'express'

import { AlbumsStore } from './store/albumsStore.js'
import type { AlbumRequest } from './types.js'

const parseAlbumId = (value: string): number | undefined => {
  const id = Number.parseInt(value, 10)
  if (Number.isNaN(id)) {
    return undefined
  }

  return id
}

export const createApp = (store: AlbumsStore = new AlbumsStore()) => {
  const app = express()

  app.use(cors())
  app.use(express.json())

  app.get('/', (_request, response) => {
    response.type('text/plain').send('Hit the /albums endpoint to retrieve a list of albums!')
  })

  app.get('/albums', (_request, response) => {
    response.json(store.list())
  })

  app.get('/albums/:id', (request, response) => {
    const id = parseAlbumId(request.params.id)
    if (id === undefined) {
      response.sendStatus(404)
      return
    }

    const album = store.getById(id)
    if (!album) {
      response.sendStatus(404)
      return
    }

    response.json(album)
  })

  app.post('/albums', (request, response) => {
    const album = store.create(request.body as AlbumRequest)
    response.location(`/albums/${album.id}`).status(201).json(album)
  })

  app.put('/albums/:id', (request, response) => {
    const id = parseAlbumId(request.params.id)
    if (id === undefined) {
      response.sendStatus(404)
      return
    }

    const album = store.update(id, request.body as AlbumRequest)
    if (!album) {
      response.sendStatus(404)
      return
    }

    response.json(album)
  })

  app.delete('/albums/:id', (request, response) => {
    const id = parseAlbumId(request.params.id)
    if (id === undefined) {
      response.sendStatus(404)
      return
    }

    const deleted = store.delete(id)
    if (!deleted) {
      response.sendStatus(404)
      return
    }

    response.sendStatus(204)
  })

  return app
}