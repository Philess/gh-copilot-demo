import { Router, Request, Response } from 'express';
import { albumService } from '../services/albumService.js';
import { CreateAlbumRequest, UpdateAlbumRequest } from '../types/album.js';

export const albumRoutes = Router();

/**
 * GET /albums
 * Retrieve all albums
 */
albumRoutes.get('/', (req: Request, res: Response) => {
  const albums = albumService.getAllAlbums();
  res.status(200).json(albums);
});

/**
 * GET /albums/search?year={year}
 * Search albums by release year
 * NOTE: This must be defined before the /:id route
 */
albumRoutes.get('/search', (req: Request, res: Response) => {
  const year = parseInt(req.query.year as string, 10);
  
  if (isNaN(year)) {
    res.status(400).json({ message: 'Invalid year parameter' });
    return;
  }

  const albums = albumService.getAlbumsByYear(year);
  res.status(200).json(albums);
});

/**
 * GET /albums/:id
 * Get a single album by ID
 */
albumRoutes.get('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    res.status(400).json({ message: 'Invalid album ID' });
    return;
  }

  const album = albumService.getAlbumById(id);
  
  if (!album) {
    res.status(404).json({ message: `Album with ID ${id} not found` });
    return;
  }

  res.status(200).json(album);
});

/**
 * POST /albums
 * Create a new album
 */
albumRoutes.post('/', (req: Request, res: Response) => {
  const request = req.body as CreateAlbumRequest;
  
  // Basic validation
  if (!request.title || !request.artist || request.price === undefined || 
      !request.year || !request.image_url) {
    res.status(400).json({ message: 'Missing required fields' });
    return;
  }

  const album = albumService.createAlbum(request);
  res.status(201)
    .location(`/albums/${album.id}`)
    .json(album);
});

/**
 * PUT /albums/:id
 * Update an existing album
 */
albumRoutes.put('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    res.status(400).json({ message: 'Invalid album ID' });
    return;
  }

  const request = req.body as UpdateAlbumRequest;
  
  // Basic validation
  if (!request.title || !request.artist || request.price === undefined || 
      !request.year || !request.image_url) {
    res.status(400).json({ message: 'Missing required fields' });
    return;
  }

  const album = albumService.updateAlbum(id, request);
  
  if (!album) {
    res.status(404).json({ message: `Album with ID ${id} not found` });
    return;
  }

  res.status(200).json(album);
});

/**
 * DELETE /albums/:id
 * Delete an album
 */
albumRoutes.delete('/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);
  
  if (isNaN(id)) {
    res.status(400).json({ message: 'Invalid album ID' });
    return;
  }

  const success = albumService.deleteAlbum(id);
  
  if (!success) {
    res.status(404).json({ message: `Album with ID ${id} not found` });
    return;
  }

  res.status(204).send();
});
