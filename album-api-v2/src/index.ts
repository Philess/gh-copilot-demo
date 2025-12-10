import express from 'express';
import cors from 'cors';
import { albums } from './data/albums';
import { Album } from './types/album';

const app = express();
app.use(cors());
app.use(express.json());

const PORT = 3000;

app.get('/', (_req, res) => {
  res.type('text/plain').send('Hit the /albums endpoint to retrieve a list of albums!');
});

app.get('/albums', (_req, res) => {
  res.json(albums);
});

app.get('/albums/:id', (req, res) => {
  const id = Number(req.params.id);
  const album = albums.find(a => a.id === id);
  if (!album) return res.status(404).json({ message: 'Album not found' });
  res.json(album);
});

function validateAlbumPayload(body: any): { valid: boolean; errors?: string[] } {
  const errors: string[] = [];
  if (typeof body.title !== 'string' || !body.title.trim()) errors.push('title is required');
  if (typeof body.artist !== 'string' || !body.artist.trim()) errors.push('artist is required');
  if (typeof body.price !== 'number' || Number.isNaN(body.price)) errors.push('price must be a number');
  if (typeof body.image_url !== 'string' || !body.image_url.trim()) errors.push('image_url is required');
  return { valid: errors.length === 0, errors };
}

app.post('/albums', (req, res) => {
  const { valid, errors } = validateAlbumPayload(req.body);
  if (!valid) return res.status(400).json({ message: 'Invalid payload', errors });

  const nextId = Math.max(0, ...albums.map(a => a.id)) + 1;
  const newAlbum: Album = { id: nextId, ...req.body };
  albums.push(newAlbum);
  res.status(201).json(newAlbum);
});

app.put('/albums/:id', (req, res) => {
  const id = Number(req.params.id);
  const idx = albums.findIndex(a => a.id === id);
  if (idx === -1) return res.status(404).json({ message: 'Album not found' });

  const { valid, errors } = validateAlbumPayload(req.body);
  if (!valid) return res.status(400).json({ message: 'Invalid payload', errors });

  const updated: Album = { id, ...req.body };
  albums[idx] = updated;
  res.json(updated);
});

app.delete('/albums/:id', (req, res) => {
  const id = Number(req.params.id);
  const idx = albums.findIndex(a => a.id === id);
  if (idx === -1) return res.status(404).json({ message: 'Album not found' });
  albums.splice(idx, 1);
  res.status(204).send();
});

if (process.env.NODE_ENV !== 'test') {
  app.listen(PORT, () => {
    console.log(`album-api-v2 listening on http://localhost:${PORT}`);
  });
}

export default app;
