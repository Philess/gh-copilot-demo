import { Router, Request, Response } from 'express';
import { albumStore } from '../models/albumStore';
import { Album } from '../types/album';

const router = Router();

// GET /albums - List all albums
router.get('/', (req: Request, res: Response) => {
  const albums = albumStore.getAll();
  res.json(albums);
});

// GET /albums/:id - Get a specific album by ID
router.get('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }

  const album = albumStore.getById(id);
  
  if (!album) {
    return res.status(404).json({ error: 'Album not found' });
  }

  res.json(album);
});

// POST /albums - Create a new album
router.post('/', (req: Request, res: Response) => {
  const { title, artist, price, image_url } = req.body;

  // Validation
  if (!title || !artist || price === undefined || !image_url) {
    return res.status(400).json({ error: 'Missing required fields: title, artist, price, image_url' });
  }

  if (typeof price !== 'number' || price < 0) {
    return res.status(400).json({ error: 'Price must be a positive number' });
  }

  const newAlbum = albumStore.create({ title, artist, price, image_url });
  res.status(201).json(newAlbum);
});

// PUT /albums/:id - Update an existing album
router.put('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);

  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }

  const { title, artist, price, image_url } = req.body;
  const updateData: Partial<Omit<Album, 'id'>> = {};

  if (title !== undefined) updateData.title = title;
  if (artist !== undefined) updateData.artist = artist;
  if (price !== undefined) {
    if (typeof price !== 'number' || price < 0) {
      return res.status(400).json({ error: 'Price must be a positive number' });
    }
    updateData.price = price;
  }
  if (image_url !== undefined) updateData.image_url = image_url;

  const updatedAlbum = albumStore.update(id, updateData);

  if (!updatedAlbum) {
    return res.status(404).json({ error: 'Album not found' });
  }

  res.json(updatedAlbum);
});

// DELETE /albums/:id - Delete an album
router.delete('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);

  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }

  const deleted = albumStore.delete(id);

  if (!deleted) {
    return res.status(404).json({ error: 'Album not found' });
  }

  res.status(204).send();
});

export default router;
