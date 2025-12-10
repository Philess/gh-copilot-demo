import request from 'supertest';
import express, { Application } from 'express';
import cors from 'cors';
import albumRoutes from '../routes/albumRoutes';

describe('Album API Endpoints', () => {
  let app: Application;

  beforeEach(() => {
    // Create a fresh app instance for each test
    app = express();
    app.use(cors());
    app.use(express.json());
    app.use('/albums', albumRoutes);
  });

  describe('GET /albums', () => {
    it('should return all albums', async () => {
      const response = await request(app)
        .get('/albums')
        .expect(200);

      expect(response.body).toBeInstanceOf(Array);
      expect(response.body.length).toBeGreaterThanOrEqual(6);
    });
  });

  describe('GET /albums/:id', () => {
    it('should return an album by id', async () => {
      const response = await request(app)
        .get('/albums/1')
        .expect(200);

      expect(response.body).toHaveProperty('id', 1);
      expect(response.body).toHaveProperty('title');
      expect(response.body).toHaveProperty('artist');
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app)
        .get('/albums/999')
        .expect(404);

      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid id', async () => {
      await request(app)
        .get('/albums/abc')
        .expect(400);
    });
  });

  describe('POST /albums', () => {
    it('should create a new album', async () => {
      const newAlbum = {
        title: 'New Album',
        artist: 'New Artist',
        price: 12.99,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(newAlbum)
        .expect(201);

      expect(response.body).toHaveProperty('id');
      expect(response.body.title).toBe(newAlbum.title);
      expect(response.body.artist).toBe(newAlbum.artist);
      expect(response.body.price).toBe(newAlbum.price);
    });

    it('should return 400 if missing required fields', async () => {
      const incompleteAlbum = {
        title: 'Incomplete Album'
      };

      await request(app)
        .post('/albums')
        .send(incompleteAlbum)
        .expect(400);
    });

    it('should return 400 if price is negative', async () => {
      const invalidAlbum = {
        title: 'Invalid Album',
        artist: 'Artist',
        price: -5,
        image_url: 'url'
      };

      await request(app)
        .post('/albums')
        .send(invalidAlbum)
        .expect(400);
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
        .expect(200);

      expect(response.body.title).toBe(updates.title);
      expect(response.body.price).toBe(updates.price);
    });

    it('should return 404 for non-existent album', async () => {
      await request(app)
        .put('/albums/999')
        .send({ title: 'Updated' })
        .expect(404);
    });

    it('should return 400 for invalid price', async () => {
      await request(app)
        .put('/albums/1')
        .send({ price: -10 })
        .expect(400);
    });
  });

  describe('DELETE /albums/:id', () => {
    it('should delete an existing album', async () => {
      await request(app)
        .delete('/albums/1')
        .expect(204);

      // Verify it's deleted
      await request(app)
        .get('/albums/1')
        .expect(404);
    });

    it('should return 404 for non-existent album', async () => {
      await request(app)
        .delete('/albums/999')
        .expect(404);
    });

    it('should return 400 for invalid id', async () => {
      await request(app)
        .delete('/albums/abc')
        .expect(400);
    });
  });
});
