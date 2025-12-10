import { Router, Request, Response } from 'express';
import {
  getAllAlbums,
  getAlbumById,
  addAlbum,
  updateAlbum,
  deleteAlbum
} from '../data/albumData';
import { Album } from '../models/album';

const router = Router();

// GET /albums - Get all albums
router.get('/', (req: Request, res: Response) => {
  const albums = getAllAlbums();
  res.json(albums);
});

// GET /albums/:id - Get album by id
router.get('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }
  
  const album = getAlbumById(id);
  
  if (!album) {
    return res.status(404).json({ error: 'Album not found' });
  }
  
  res.json(album);
});

// POST /albums - Create a new album
router.post('/', (req: Request, res: Response) => {
  const { title, artist, price, image_url } = req.body;
  
  if (!title || !artist || price === undefined || !image_url) {
    return res.status(400).json({ error: 'Missing required fields: title, artist, price, image_url' });
  }
  
  if (typeof price !== 'number' || price < 0) {
    return res.status(400).json({ error: 'Price must be a positive number' });
  }
  
  const newAlbum = addAlbum({ title, artist, price, image_url });
  res.status(201).json(newAlbum);
});

// PUT /albums/:id - Update an album
router.put('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }
  
  const { title, artist, price, image_url } = req.body;
  
  const updates: Partial<Omit<Album, 'id'>> = {};
  if (title !== undefined) updates.title = title;
  if (artist !== undefined) updates.artist = artist;
  if (price !== undefined) {
    if (typeof price !== 'number' || price < 0) {
      return res.status(400).json({ error: 'Price must be a positive number' });
    }
    updates.price = price;
  }
  if (image_url !== undefined) updates.image_url = image_url;
  
  const updatedAlbum = updateAlbum(id, updates);
  
  if (!updatedAlbum) {
    return res.status(404).json({ error: 'Album not found' });
  }
  
  res.json(updatedAlbum);
});

// DELETE /albums/:id - Delete an album
router.delete('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }
  
  const deleted = deleteAlbum(id);
  
  if (!deleted) {
    return res.status(404).json({ error: 'Album not found' });
  }
  
  res.status(204).send();
});

export default router;
