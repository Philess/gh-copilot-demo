import { Router } from 'express';
import { albums } from '../data/sampleData';
import { Album } from '../models/album';

const router = Router();

// GET /albums?sort=title|artist|price&year=2020
router.get('/', (req, res) => {
  let result: Album[] = [...albums];

  const sort = (req.query.sort as string) || null;
  const year = req.query.year ? Number(req.query.year) : null;

  if (year) {
    result = result.filter(a => a.Year === year);
  }

  if (sort) {
    if (sort === 'title') result.sort((a, b) => a.Title.localeCompare(b.Title));
    else if (sort === 'artist') result.sort((a, b) => a.Artist.Name.localeCompare(b.Artist.Name));
    else if (sort === 'price') result.sort((a, b) => a.Price - b.Price);
  }

  res.json(result);
});

// GET /albums/:id
router.get('/:id', (req, res) => {
  const id = Number(req.params.id);
  const album = albums.find(a => a.Id === id);
  if (!album) return res.status(404).send({ message: 'Album not found' });
  res.json(album);
});

// POST /albums
router.post('/', (req, res) => {
  const body = req.body as Partial<Album> | null;
  if (!body) return res.status(400).send({ message: 'Album is required' });

  const nextId = Math.max(...albums.map(a => a.Id), 0) + 1;
  const newAlbum: Album = {
    Id: nextId,
    Title: body.Title || '',
    Artist: body.Artist as any || { Name: '', Birthdate: new Date().toISOString(), BirthPlace: '' },
    Year: body.Year || new Date().getFullYear(),
    Price: body.Price || 0,
    Image_url: body.Image_url || ''
  };

  albums.push(newAlbum);
  res.status(201).json(newAlbum);
});

// PUT /albums/:id
router.put('/:id', (req, res) => {
  const id = Number(req.params.id);
  const index = albums.findIndex(a => a.Id === id);
  if (index === -1) return res.status(404).send({ message: 'Album not found' });

  const body = req.body as Partial<Album> | null;
  if (!body) return res.status(400).send({ message: 'Album is required' });

  const updated: Album = {
    Id: id,
    Title: body.Title ?? albums[index].Title,
    Artist: (body.Artist as any) ?? albums[index].Artist,
    Year: body.Year ?? albums[index].Year,
    Price: body.Price ?? albums[index].Price,
    Image_url: body.Image_url ?? albums[index].Image_url
  };

  albums[index] = updated;
  res.json(updated);
});

// DELETE /albums/:id
router.delete('/:id', (req, res) => {
  const id = Number(req.params.id);
  const index = albums.findIndex(a => a.Id === id);
  if (index === -1) return res.status(404).send({ message: 'Album not found' });
  albums.splice(index, 1);
  res.status(204).send();
});

export default router;
