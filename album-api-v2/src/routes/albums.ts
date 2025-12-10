import { Router, Request, Response } from 'express';
import { albums } from '../data/albums';
import { Album } from '../types/album';

const router = Router();

router.get('/', (_req: Request, res: Response) => {
  res.type('text/plain').send('Hit the /albums endpoint to retrieve a list of albums!');
});

router.get('/albums', (_req: Request, res: Response) => {
  res.json(albums);
});

router.get('/albums/:id', (req: Request, res: Response) => {
  const id = Number(req.params.id);
  const found = albums.find(a => a.id === id);
  if (!found) return res.status(404).json({ message: 'Album not found' });
  res.json(found);
});

router.post('/albums', (req: Request, res: Response) => {
  const body = req.body as Partial<Album>;
  if (!body || typeof body.title !== 'string' || typeof body.artist !== 'string' || typeof body.year !== 'number' || typeof body.genre !== 'string' || typeof body.price !== 'number') {
    return res.status(400).json({ message: 'Invalid album payload' });
  }
  const id = albums.length ? Math.max(...albums.map(a => a.id)) + 1 : 1;
  const album: Album = { id, title: body.title, artist: body.artist, year: body.year, genre: body.genre, price: body.price };
  albums.push(album);
  res.status(201).json(album);
});

router.put('/albums/:id', (req: Request, res: Response) => {
  const id = Number(req.params.id);
  const idx = albums.findIndex(a => a.id === id);
  if (idx === -1) return res.status(404).json({ message: 'Album not found' });
  const body = req.body as Partial<Album>;
  const current = albums[idx];
  const updated: Album = {
    id: current.id,
    title: typeof body.title === 'string' ? body.title : current.title,
    artist: typeof body.artist === 'string' ? body.artist : current.artist,
    year: typeof body.year === 'number' ? body.year : current.year,
    genre: typeof body.genre === 'string' ? body.genre : current.genre,
    price: typeof body.price === 'number' ? body.price : current.price,
  };
  albums[idx] = updated;
  res.json(updated);
});

router.delete('/albums/:id', (req: Request, res: Response) => {
  const id = Number(req.params.id);
  const idx = albums.findIndex(a => a.id === id);
  if (idx === -1) return res.status(404).json({ message: 'Album not found' });
  const [deleted] = albums.splice(idx, 1);
  res.json(deleted);
});

export default router;
