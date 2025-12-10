import { Router, Request, Response } from 'express';
import { albumStore } from '../data/albumStore';
import { Album } from '../models/Album';

const router = Router();

/**
 * GET /albums - List all albums
 */
router.get('/', (req: Request, res: Response) => {
  const albums = albumStore.getAllAlbums();
  res.json(albums);
});

/**
 * GET /albums/:id - Get single album by ID
 */
router.get('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }

  const album = albumStore.getAlbumById(id);
  
  if (!album) {
    return res.status(404).json({ error: 'Album not found' });
  }

  res.json(album);
});

/**
 * POST /albums - Create new album
 */
router.post('/', (req: Request, res: Response) => {
  const { title, artist, price, image_url } = req.body;

  // Validate required fields
  if (!title || !artist || price === undefined || !image_url) {
    return res.status(400).json({ 
      error: 'Missing required fields: title, artist, price, image_url' 
    });
  }

  // Validate price
  if (typeof price !== 'number' || price <= 0) {
    return res.status(400).json({ 
      error: 'Price must be a positive number' 
    });
  }

  const newAlbum = albumStore.addAlbum({
    title,
    artist,
    price,
    image_url
  });

  res.status(201).json(newAlbum);
});

/**
 * PUT /albums/:id - Update existing album
 */
router.put('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }

  const { title, artist, price, image_url } = req.body;

  // Validate price if provided
  if (price !== undefined && (typeof price !== 'number' || price <= 0)) {
    return res.status(400).json({ 
      error: 'Price must be a positive number' 
    });
  }

  const updates: Partial<Omit<Album, 'id'>> = {};
  if (title !== undefined) updates.title = title;
  if (artist !== undefined) updates.artist = artist;
  if (price !== undefined) updates.price = price;
  if (image_url !== undefined) updates.image_url = image_url;

  const updatedAlbum = albumStore.updateAlbum(id, updates);

  if (!updatedAlbum) {
    return res.status(404).json({ error: 'Album not found' });
  }

  res.json(updatedAlbum);
});

/**
 * DELETE /albums/:id - Delete album
 */
router.delete('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    return res.status(400).json({ error: 'Invalid album ID' });
  }

  const deleted = albumStore.deleteAlbum(id);

  if (!deleted) {
    return res.status(404).json({ error: 'Album not found' });
  }

  res.status(204).send();
});

export default router;
