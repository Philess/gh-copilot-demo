import { Router, Request, Response } from 'express'
import * as albumStore from '../data/albumStore'
import { AlbumInput } from '../models/album'

const router = Router()

function parseIntStrict(value: string): number | null {
  return /^-?\d+$/.test(value) ? parseInt(value, 10) : null
}

// must be registered before "/:id" so "sort" isn't treated as an id
router.get('/sort', (req: Request, res: Response) => {
  const sortBy = String(req.query.sortBy ?? '').toLowerCase()
  const albums = albumStore.getAll()

  switch (sortBy) {
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
      return res.status(400).json("Invalid sort parameter. Please use 'title', 'artist', or 'price'.")
  }

  res.json(albums)
})

// must be registered before "/:id" so "year" isn't treated as an id
router.get('/year/:year', (req: Request, res: Response) => {
  const year = parseIntStrict(req.params.year)
  if (year === null) {
    return res.status(400).json('Invalid year parameter.')
  }

  res.json(albumStore.getByYear(year))
})

router.get('/', (_req: Request, res: Response) => {
  res.json(albumStore.getAll())
})

router.post('/', (req: Request, res: Response) => {
  const input = req.body as AlbumInput
  const createdAlbum = albumStore.create(input)
  res.status(201).location(`/albums/${createdAlbum.id}`).json(createdAlbum)
})

router.get('/:id', (req: Request, res: Response) => {
  const id = parseIntStrict(req.params.id)
  if (id === null) {
    return res.status(400).json('Invalid id parameter.')
  }

  const album = albumStore.getById(id)
  if (!album) {
    return res.status(404).end()
  }

  res.json(album)
})

router.put('/:id', (req: Request, res: Response) => {
  const id = parseIntStrict(req.params.id)
  if (id === null) {
    return res.status(400).json('Invalid id parameter.')
  }

  const input = req.body as AlbumInput
  const updatedAlbum = albumStore.update(id, input)
  if (!updatedAlbum) {
    return res.status(404).end()
  }

  res.json(updatedAlbum)
})

router.delete('/:id', (req: Request, res: Response) => {
  const id = parseIntStrict(req.params.id)
  if (id === null) {
    return res.status(400).json('Invalid id parameter.')
  }

  const deleted = albumStore.remove(id)
  res.status(deleted ? 204 : 404).end()
})

export default router
