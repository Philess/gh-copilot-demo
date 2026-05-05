import { Router, Request, Response } from 'express'
import * as store from '../data/store'
import { AlbumRequest } from '../types/album'

const router = Router()

// GET /albums/sort?sortBy=title|artist|price
// Must be registered before /:id to prevent "sort" matching the id parameter.
router.get('/sort', (req: Request, res: Response): void => {
  const sortBy = req.query.sortBy as string | undefined

  if (!sortBy || sortBy.trim() === '') {
    res.status(400).json({ error: 'sortBy parameter is required. Valid values are: title, artist, price.' })
    return
  }

  const albums = store.getAll()
  switch (sortBy.toLowerCase()) {
    case 'title':
      albums.sort((a, b) => a.title.localeCompare(b.title))
      break
    case 'artist':
      albums.sort((a, b) => a.artist.name.localeCompare(b.artist.name))
      break
    case 'price':
      albums.sort((a, b) => a.price - b.price)
      break
    default:
      res.status(400).json({ error: 'Invalid sortBy parameter. Valid values are: title, artist, price.' })
      return
  }

  res.json(albums)
})

// GET /albums/search?year=2021
// Must be registered before /:id to prevent "search" matching the id parameter.
router.get('/search', (req: Request, res: Response): void => {
  const year = parseInt(req.query.year as string, 10)
  res.json(store.getByYear(year))
})

// GET /albums
router.get('/', (_req: Request, res: Response): void => {
  res.json(store.getAll())
})

// GET /albums/:id
router.get('/:id', (req: Request, res: Response): void => {
  const id = parseInt(req.params.id, 10)
  const album = store.getById(id)
  if (!album) {
    res.status(404).json({ error: 'Album not found' })
    return
  }
  res.json(album)
})

// POST /albums
router.post('/', (req: Request, res: Response): void => {
  const body = req.body as AlbumRequest
  const album = store.create({
    title: body.title,
    artist: body.artist,
    year: body.year,
    price: body.price,
    image_url: body.image_url,
  })
  res.status(201).location(`/albums/${album.id}`).json(album)
})

// PUT /albums/:id
router.put('/:id', (req: Request, res: Response): void => {
  const id = parseInt(req.params.id, 10)
  const body = req.body as AlbumRequest
  const album = store.update(id, {
    title: body.title,
    artist: body.artist,
    year: body.year,
    price: body.price,
    image_url: body.image_url,
  })
  if (!album) {
    res.status(404).json({ error: 'Album not found' })
    return
  }
  res.json(album)
})

// DELETE /albums/:id
router.delete('/:id', (req: Request, res: Response): void => {
  const id = parseInt(req.params.id, 10)
  if (!store.remove(id)) {
    res.status(404).json({ error: 'Album not found' })
    return
  }
  res.sendStatus(204)
})

export default router
