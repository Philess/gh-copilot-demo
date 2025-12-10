import { describe, it, expect, beforeEach } from 'vitest';
import request from 'supertest';
import app from '../app.js';
import { resetAlbums, albums } from '../data/albums.js';

describe('Album API v2', () => {
  // Reset albums before each test
  beforeEach(() => {
    resetAlbums();
  });

  describe('GET /', () => {
    it('should return welcome message', async () => {
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
      expect(response.body[0]).toHaveProperty('image_url');
    });

    it('should return the correct first album', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.body[0].id).toBe(1);
      expect(response.body[0].title).toBe('You, Me and an App Id');
      expect(response.body[0].artist).toBe('Daprize');
      expect(response.body[0].price).toBe(10.99);
    });
  });

  describe('GET /albums/:id', () => {
    it('should return an album by ID', async () => {
      const response = await request(app).get('/albums/1');
      
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(1);
      expect(response.body.title).toBe('You, Me and an App Id');
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).get('/albums/999');
      
      expect(response.status).toBe(404);
      expect(response.body.error).toBe('Album not found');
    });

    it('should return 400 for invalid ID', async () => {
      const response = await request(app).get('/albums/invalid');
      
      expect(response.status).toBe(400);
      expect(response.body.error).toBe('Invalid album ID');
    });
  });

  describe('POST /albums', () => {
    it('should create a new album', async () => {
      const newAlbum = {
        title: 'New Album',
        artist: 'New Artist',
        price: 9.99,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(newAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(201);
      expect(response.body.id).toBe(7);
      expect(response.body.title).toBe('New Album');
      expect(response.body.artist).toBe('New Artist');
      expect(albums).toHaveLength(7);
    });

    it('should return 400 when missing required fields', async () => {
      const invalidAlbum = {
        title: 'Incomplete Album'
      };

      const response = await request(app)
        .post('/albums')
        .send(invalidAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.error).toContain('Missing required fields');
    });

    it('should return 400 for negative price', async () => {
      const invalidAlbum = {
        title: 'Bad Album',
        artist: 'Bad Artist',
        price: -5,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(invalidAlbum)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.error).toBe('Price must be a positive number');
    });
  });

  describe('PUT /albums/:id', () => {
    it('should update an existing album', async () => {
      const updates = {
        title: 'Updated Title',
        price: 19.99
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updates)
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(1);
      expect(response.body.title).toBe('Updated Title');
      expect(response.body.price).toBe(19.99);
      // Original fields should be preserved
      expect(response.body.artist).toBe('Daprize');
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app)
        .put('/albums/999')
        .send({ title: 'Test' })
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(404);
      expect(response.body.error).toBe('Album not found');
    });

    it('should return 400 for invalid ID', async () => {
      const response = await request(app)
        .put('/albums/invalid')
        .send({ title: 'Test' })
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.error).toBe('Invalid album ID');
    });

    it('should return 400 for negative price', async () => {
      const response = await request(app)
        .put('/albums/1')
        .send({ price: -10 })
        .set('Content-Type', 'application/json');
      
      expect(response.status).toBe(400);
      expect(response.body.error).toBe('Price must be a positive number');
    });
  });

  describe('DELETE /albums/:id', () => {
    it('should delete an existing album', async () => {
      const response = await request(app).delete('/albums/1');
      
      expect(response.status).toBe(204);
      expect(albums).toHaveLength(5);
      expect(albums.find(a => a.id === 1)).toBeUndefined();
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).delete('/albums/999');
      
      expect(response.status).toBe(404);
      expect(response.body.error).toBe('Album not found');
    });

    it('should return 400 for invalid ID', async () => {
      const response = await request(app).delete('/albums/invalid');
      
      expect(response.status).toBe(400);
      expect(response.body.error).toBe('Invalid album ID');
    });
  });
});
