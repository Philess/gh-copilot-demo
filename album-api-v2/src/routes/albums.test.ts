import request from 'supertest';
import express, { Application } from 'express';
import albumRoutes from '../routes/albums';

describe('Album API Routes', () => {
  let app: Application;

  beforeEach(() => {
    // Create a fresh Express app for each test
    app = express();
    app.use(express.json());
    app.use('/albums', albumRoutes);
  });

  describe('GET /albums', () => {
    it('should return all albums', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.status).toBe(200);
      expect(Array.isArray(response.body)).toBe(true);
      expect(response.body.length).toBeGreaterThan(0);
    });

    it('should return albums with correct structure', async () => {
      const response = await request(app).get('/albums');
      
      const album = response.body[0];
      expect(album).toHaveProperty('id');
      expect(album).toHaveProperty('title');
      expect(album).toHaveProperty('artist');
      expect(album).toHaveProperty('price');
      expect(album).toHaveProperty('image_url');
    });
  });

  describe('GET /albums/:id', () => {
    it('should return a specific album by ID', async () => {
      const response = await request(app).get('/albums/1');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('id', 1);
      expect(response.body).toHaveProperty('title');
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).get('/albums/9999');
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid ID', async () => {
      const response = await request(app).get('/albums/invalid');
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error');
    });
  });

  describe('POST /albums', () => {
    it('should create a new album', async () => {
      const newAlbum = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: 19.99,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(newAlbum);
      
      expect(response.status).toBe(201);
      expect(response.body).toHaveProperty('id');
      expect(response.body.title).toBe(newAlbum.title);
      expect(response.body.artist).toBe(newAlbum.artist);
      expect(response.body.price).toBe(newAlbum.price);
    });

    it('should return 400 for missing fields', async () => {
      const invalidAlbum = {
        title: 'Test Album'
        // missing artist, price, image_url
      };

      const response = await request(app)
        .post('/albums')
        .send(invalidAlbum);
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid price', async () => {
      const invalidAlbum = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: -10,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(invalidAlbum);
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error');
    });
  });

  describe('PUT /albums/:id', () => {
    it('should update an existing album', async () => {
      const updateData = {
        title: 'Updated Title',
        price: 24.99
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updateData);
      
      expect(response.status).toBe(200);
      expect(response.body.title).toBe(updateData.title);
      expect(response.body.price).toBe(updateData.price);
    });

    it('should return 404 for non-existent album', async () => {
      const updateData = { title: 'Updated Title' };

      const response = await request(app)
        .put('/albums/9999')
        .send(updateData);
      
      expect(response.status).toBe(404);
    });

    it('should return 400 for invalid price in update', async () => {
      const updateData = { price: -5 };

      const response = await request(app)
        .put('/albums/1')
        .send(updateData);
      
      expect(response.status).toBe(400);
    });
  });

  describe('DELETE /albums/:id', () => {
    it('should delete an existing album', async () => {
      const response = await request(app).delete('/albums/1');
      
      expect(response.status).toBe(204);
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).delete('/albums/9999');
      
      expect(response.status).toBe(404);
    });

    it('should return 400 for invalid ID', async () => {
      const response = await request(app).delete('/albums/invalid');
      
      expect(response.status).toBe(400);
    });
  });
});
