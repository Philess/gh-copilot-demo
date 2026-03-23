import express, { Router, Request, Response } from 'express';
import { getAllAlbums, getAlbumById, addAlbum, updateAlbum, deleteAlbum, Album } from '../models/album';

const router: Router = express.Router();

// GET /albums - Get all albums
router.get('/', (_req: Request, res: Response) => {
  const albums = getAllAlbums();
  res.json(albums);
});

// GET /albums/:id - Get album by ID
router.get('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);

  if (isNaN(id)) {
    res.status(400).json({ error: 'Invalid album ID' });
    return;
  }

  const album = getAlbumById(id);
  if (!album) {
    res.status(404).json({ error: 'Album not found' });
    return;
  }

  res.json(album);
});

// POST /albums - Create new album
router.post('/', (req: Request, res: Response) => {
  const { title, artist, price, image_url } = req.body;

  // Validation
  if (!title || typeof title !== 'string' || title.trim() === '') {
    res.status(400).json({ error: 'Missing or invalid required field: title' });
    return;
  }

  if (!artist || typeof artist !== 'string' || artist.trim() === '') {
    res.status(400).json({ error: 'Missing or invalid required field: artist' });
    return;
  }

  if (price === undefined || typeof price !== 'number' || price < 0) {
    res.status(400).json({ error: 'Missing or invalid required field: price' });
    return;
  }

  if (!image_url || typeof image_url !== 'string' || image_url.trim() === '') {
    res.status(400).json({ error: 'Missing or invalid required field: image_url' });
    return;
  }

  try {
    const newAlbum = addAlbum({ title, artist, price, image_url });
    res.status(201).json(newAlbum);
  } catch (error) {
    res.status(500).json({ error: 'Failed to create album' });
  }
});

// PUT /albums/:id - Update album
router.put('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);

  if (isNaN(id)) {
    res.status(400).json({ error: 'Invalid album ID' });
    return;
  }

  const { title, artist, price, image_url } = req.body;

  // Validation
  if (!title || typeof title !== 'string' || title.trim() === '') {
    res.status(400).json({ error: 'Missing or invalid required field: title' });
    return;
  }

  if (!artist || typeof artist !== 'string' || artist.trim() === '') {
    res.status(400).json({ error: 'Missing or invalid required field: artist' });
    return;
  }

  if (price === undefined || typeof price !== 'number' || price < 0) {
    res.status(400).json({ error: 'Missing or invalid required field: price' });
    return;
  }

  if (!image_url || typeof image_url !== 'string' || image_url.trim() === '') {
    res.status(400).json({ error: 'Missing or invalid required field: image_url' });
    return;
  }

  try {
    const updatedAlbum = updateAlbum(id, { title, artist, price, image_url });
    if (!updatedAlbum) {
      res.status(404).json({ error: 'Album not found' });
      return;
    }

    res.json(updatedAlbum);
  } catch (error) {
    res.status(500).json({ error: 'Failed to update album' });
  }
});

// DELETE /albums/:id - Delete album
router.delete('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);

  if (isNaN(id)) {
    res.status(400).json({ error: 'Invalid album ID' });
    return;
  }

  try {
    const deletedAlbum = deleteAlbum(id);
    if (!deletedAlbum) {
      res.status(404).json({ error: 'Album not found' });
      return;
    }

    res.status(204).send();
  } catch (error) {
    res.status(500).json({ error: 'Failed to delete album' });
  }
});

export default router;
