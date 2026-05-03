import request from 'supertest';
import { createApp } from '../app.js';
import { albums, idCounter } from '../data/albums.js';

describe('Album Routes', () => {
  const app = createApp();
  let initialAlbums: any[];
  let initialNextId: number;

  beforeEach(() => {
    // Store initial state
    initialAlbums = [...albums];
    initialNextId = idCounter.nextId;
  });

  afterEach(() => {
    // Restore initial state after each test
    albums.length = 0;
    albums.push(...initialAlbums);
    idCounter.nextId = initialNextId;
  });

  describe('GET /', () => {
    it('should return the welcome message', async () => {
      const response = await request(app).get('/');
      
      expect(response.status).toBe(200);
      expect(response.text).toBe('Hit the /albums endpoint to retrieve a list of albums!');
    });
  });

  describe('GET /albums', () => {
    it('should return all albums', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveLength(6);
      expect(response.body[0]).toHaveProperty('id');
      expect(response.body[0]).toHaveProperty('title');
      expect(response.body[0]).toHaveProperty('artist');
      expect(response.body[0]).toHaveProperty('price');
      expect(response.body[0]).toHaveProperty('year');
      expect(response.body[0]).toHaveProperty('image_url');
    });

    it('should return albums with correct data', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.body[0].title).toBe('You, Me and an App Id');
      expect(response.body[0].artist).toBe('Daprize');
      expect(response.body[0].year).toBe(2021);
    });
  });

  describe('GET /albums/:id', () => {
    it('should return a specific album', async () => {
      const response = await request(app).get('/albums/1');
      
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(1);
      expect(response.body.title).toBe('You, Me and an App Id');
      expect(response.body.artist).toBe('Daprize');
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).get('/albums/999');
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('message');
      expect(response.body.message).toContain('Album with ID 999 not found');
    });

    it('should return 400 for invalid album ID', async () => {
      const response = await request(app).get('/albums/abc');
      
      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Invalid album ID');
    });
  });

  describe('GET /albums/search', () => {
    it('should return albums from specified year', async () => {
      const response = await request(app).get('/albums/search?year=2021');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveLength(4);
      response.body.forEach((album: any) => {
        expect(album.year).toBe(2021);
      });
    });

    it('should return empty array for year with no albums', async () => {
      const response = await request(app).get('/albums/search?year=1999');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveLength(0);
    });

    it('should return albums from 2020', async () => {
      const response = await request(app).get('/albums/search?year=2020');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveLength(2);
    });

    it('should return 400 for invalid year parameter', async () => {
      const response = await request(app).get('/albums/search?year=abc');
      
      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Invalid year parameter');
    });

    it('should return 400 for missing year parameter', async () => {
      const response = await request(app).get('/albums/search');
      
      expect(response.status).toBe(400);
    });
  });

  describe('POST /albums', () => {
    it('should create a new album', async () => {
      const newAlbum = {
        title: 'New Test Album',
        artist: 'Test Artist',
        price: 15.99,
        year: 2023,
        image_url: 'https://example.com/test.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(newAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(201);
      expect(response.body.id).toBe(7);
      expect(response.body.title).toBe(newAlbum.title);
      expect(response.body.artist).toBe(newAlbum.artist);
      expect(response.body.price).toBe(newAlbum.price);
      expect(response.header.location).toBe('/albums/7');
    });

    it('should increment ID for each new album', async () => {
      const album1 = {
        title: 'Album 1',
        artist: 'Artist 1',
        price: 10.0,
        year: 2023,
        image_url: 'https://example.com/1.jpg'
      };

      const album2 = {
        title: 'Album 2',
        artist: 'Artist 2',
        price: 11.0,
        year: 2023,
        image_url: 'https://example.com/2.jpg'
      };

      const response1 = await request(app).post('/albums').send(album1);
      const response2 = await request(app).post('/albums').send(album2);
      
      expect(response1.body.id).toBe(7);
      expect(response2.body.id).toBe(8);
    });

    it('should return 400 for missing required fields', async () => {
      const incompleteAlbum = {
        title: 'Test Album',
        artist: 'Test Artist'
        // Missing price, year, image_url
      };

      const response = await request(app)
        .post('/albums')
        .send(incompleteAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Missing required fields');
    });
  });

  describe('PUT /albums/:id', () => {
    it('should update an existing album', async () => {
      const updatedAlbum = {
        title: 'Updated Title',
        artist: 'Updated Artist',
        price: 99.99,
        year: 2024,
        image_url: 'https://example.com/updated.jpg'
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updatedAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(1);
      expect(response.body.title).toBe('Updated Title');
      expect(response.body.artist).toBe('Updated Artist');
      expect(response.body.price).toBe(99.99);
    });

    it('should return 404 for non-existent album', async () => {
      const updatedAlbum = {
        title: 'Test',
        artist: 'Test',
        price: 10.0,
        year: 2023,
        image_url: 'https://example.com/test.jpg'
      };

      const response = await request(app)
        .put('/albums/999')
        .send(updatedAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(404);
      expect(response.body.message).toContain('Album with ID 999 not found');
    });

    it('should return 400 for missing required fields', async () => {
      const incompleteAlbum = {
        title: 'Test Album'
        // Missing other fields
      };

      const response = await request(app)
        .put('/albums/1')
        .send(incompleteAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Missing required fields');
    });

    it('should return 400 for invalid album ID', async () => {
      const updatedAlbum = {
        title: 'Test',
        artist: 'Test',
        price: 10.0,
        year: 2023,
        image_url: 'https://example.com/test.jpg'
      };

      const response = await request(app)
        .put('/albums/abc')
        .send(updatedAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Invalid album ID');
    });
  });

  describe('DELETE /albums/:id', () => {
    it('should delete an existing album', async () => {
      const response = await request(app).delete('/albums/1');
      
      expect(response.status).toBe(204);
      expect(response.body).toEqual({});
      
      // Verify album is deleted
      const getResponse = await request(app).get('/albums/1');
      expect(getResponse.status).toBe(404);
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).delete('/albums/999');
      
      expect(response.status).toBe(404);
      expect(response.body.message).toContain('Album with ID 999 not found');
    });

    it('should return 400 for invalid album ID', async () => {
      const response = await request(app).delete('/albums/abc');
      
      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Invalid album ID');
    });

    it('should not affect other albums when deleting', async () => {
      await request(app).delete('/albums/1');
      
      const response = await request(app).get('/albums/2');
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(2);
    });
  });
});
