import { Request, Response } from 'express';
import { albumStore } from '../models/album';

export class AlbumController {
  // GET /albums - Get all albums
  getAll(req: Request, res: Response): void {
    const albums = albumStore.getAll();
    res.json(albums);
  }

  // GET /albums/:id - Get album by ID
  getById(req: Request, res: Response): void {
    const id = parseInt(req.params.id);
    
    if (isNaN(id)) {
      res.status(400).json({ error: 'Invalid album ID' });
      return;
    }

    const album = albumStore.getById(id);
    
    if (!album) {
      res.status(404).json({ error: 'Album not found' });
      return;
    }

    res.json(album);
  }

  // POST /albums - Create a new album
  create(req: Request, res: Response): void {
    const { title, artist, price, image_url } = req.body;

    if (!title || !artist || price === undefined || !image_url) {
      res.status(400).json({ error: 'Missing required fields: title, artist, price, image_url' });
      return;
    }

    if (typeof price !== 'number' || price < 0) {
      res.status(400).json({ error: 'Price must be a positive number' });
      return;
    }

    const newAlbum = albumStore.create({ title, artist, price, image_url });
    res.status(201).json(newAlbum);
  }

  // PUT /albums/:id - Update an album
  update(req: Request, res: Response): void {
    const id = parseInt(req.params.id);
    
    if (isNaN(id)) {
      res.status(400).json({ error: 'Invalid album ID' });
      return;
    }

    const { title, artist, price, image_url } = req.body;
    
    if (price !== undefined && (typeof price !== 'number' || price < 0)) {
      res.status(400).json({ error: 'Price must be a positive number' });
      return;
    }

    const updatedAlbum = albumStore.update(id, { title, artist, price, image_url });
    
    if (!updatedAlbum) {
      res.status(404).json({ error: 'Album not found' });
      return;
    }

    res.json(updatedAlbum);
  }

  // DELETE /albums/:id - Delete an album
  delete(req: Request, res: Response): void {
    const id = parseInt(req.params.id);
    
    if (isNaN(id)) {
      res.status(400).json({ error: 'Invalid album ID' });
      return;
    }

    const deleted = albumStore.delete(id);
    
    if (!deleted) {
      res.status(404).json({ error: 'Album not found' });
      return;
    }

    res.status(204).send();
  }
}

export const albumController = new AlbumController();
