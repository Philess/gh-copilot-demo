import express, { Request, Response } from 'express';
import { albums, Album } from './album';

const app = express();
app.use(express.json());

// GET /albums - List all albums
app.get('/albums', (req: Request, res: Response) => {
  res.json(albums);
});

// GET /albums/:id - Get album by id
app.get('/albums/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  const album = albums.find(a => a.id === id);
  if (!album) return res.status(404).send();
  res.json(album);
});

// POST /albums - Add new album
app.post('/albums', (req: Request, res: Response) => {
  const { title, artist, year } = req.body;
  if (!title || !artist || typeof year !== 'number') {
    return res.status(400).json({ error: 'Invalid album data' });
  }
  const id = albums.length ? Math.max(...albums.map(a => a.id)) + 1 : 1;
  const album: Album = { id, title, artist, year };
  albums.push(album);
  res.status(201).json(album);
});

// PUT /albums/:id - Update album
app.put('/albums/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  const index = albums.findIndex(a => a.id === id);
  if (index === -1) return res.status(404).send();
  const { title, artist, year } = req.body;
  if (!title || !artist || typeof year !== 'number') {
    return res.status(400).json({ error: 'Invalid album data' });
  }
  albums[index] = { id, title, artist, year };
  res.status(204).send();
});

// DELETE /albums/:id - Delete album
app.delete('/albums/:id', (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  const index = albums.findIndex(a => a.id === id);
  if (index === -1) return res.status(404).send();
  albums.splice(index, 1);
  res.status(204).send();
});

// GET /albums/search?year=YYYY - Search by year
app.get('/albums/search', (req: Request, res: Response) => {
  const year = parseInt(req.query.year as string);
  if (isNaN(year)) return res.status(400).json({ error: 'Invalid year' });
  const filtered = albums.filter(a => a.year === year);
  res.json(filtered);
});

const PORT = 3000;
app.listen(PORT, () => {
  console.log(`Album API v2 listening on port ${PORT}`);
});
