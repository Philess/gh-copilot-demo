import { Router, Request, Response } from 'express';
import { Album, CreateAlbumDto, UpdateAlbumDto } from '../models/album.js';
import { albums, getNextId } from '../data/albums.js';

const router = Router();

// GET /albums - List all albums
router.get('/', (_req: Request, res: Response) => {
  res.json(albums);
});

// GET /albums/:id - Get album by ID
router.get('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    res.status(400).json({ error: 'Invalid album ID' });
    return;
  }
  
  const album = albums.find(a => a.id === id);
  
  if (!album) {
    res.status(404).json({ error: 'Album not found' });
    return;
  }
  
  res.json(album);
});

// POST /albums - Create a new album
router.post('/', (req: Request, res: Response) => {
  const body = req.body as CreateAlbumDto;
  
  // Validate required fields
  if (!body.title || !body.artist || body.price === undefined || !body.image_url) {
    res.status(400).json({ 
      error: 'Missing required fields. Required: title, artist, price, image_url' 
    });
    return;
  }
  
  if (typeof body.price !== 'number' || body.price < 0) {
    res.status(400).json({ error: 'Price must be a positive number' });
    return;
  }
  
  const newAlbum: Album = {
    id: getNextId(),
    title: body.title,
    artist: body.artist,
    price: body.price,
    image_url: body.image_url
  };
  
  albums.push(newAlbum);
  res.status(201).json(newAlbum);
});

// PUT /albums/:id - Update an album
router.put('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    res.status(400).json({ error: 'Invalid album ID' });
    return;
  }
  
  const albumIndex = albums.findIndex(a => a.id === id);
  
  if (albumIndex === -1) {
    res.status(404).json({ error: 'Album not found' });
    return;
  }
  
  const body = req.body as UpdateAlbumDto;
  const existingAlbum = albums[albumIndex];
  
  // Validate price if provided
  if (body.price !== undefined && (typeof body.price !== 'number' || body.price < 0)) {
    res.status(400).json({ error: 'Price must be a positive number' });
    return;
  }
  
  const updatedAlbum: Album = {
    id: existingAlbum.id,
    title: body.title ?? existingAlbum.title,
    artist: body.artist ?? existingAlbum.artist,
    price: body.price ?? existingAlbum.price,
    image_url: body.image_url ?? existingAlbum.image_url
  };
  
  albums[albumIndex] = updatedAlbum;
  res.json(updatedAlbum);
});

// DELETE /albums/:id - Delete an album
router.delete('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    res.status(400).json({ error: 'Invalid album ID' });
    return;
  }
  
  const albumIndex = albums.findIndex(a => a.id === id);
  
  if (albumIndex === -1) {
    res.status(404).json({ error: 'Album not found' });
    return;
  }
  
  albums.splice(albumIndex, 1);
  res.status(204).send();
});

export default router;
