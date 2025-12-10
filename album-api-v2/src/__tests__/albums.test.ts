import request from 'supertest';
import express from 'express';
import albumRoutes from '../routes/albums';
import { albumStore } from '../data/albumStore';

// Create test app
const app = express();
app.use(express.json());
app.use('/albums', albumRoutes);

describe('Album API Endpoints', () => {
  // Reset data store before each test
  beforeEach(() => {
    albumStore.reset();
  });

  describe('GET /albums', () => {
    it('should return 200 and array of albums', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.status).toBe(200);
      expect(Array.isArray(response.body)).toBe(true);
      expect(response.body).toHaveLength(6);
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

    it('should return correct sample data', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.body[0].title).toBe('You, Me and an App Id');
      expect(response.body[0].artist).toBe('Daprize');
      expect(response.body[0].price).toBe(10.99);
    });
  });

  describe('GET /albums/:id', () => {
    it('should return 200 and specific album for valid ID', async () => {
      const response = await request(app).get('/albums/1');
      
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(1);
      expect(response.body.title).toBe('You, Me and an App Id');
    });

    it('should return 404 for non-existent ID', async () => {
      const response = await request(app).get('/albums/999');
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid ID format', async () => {
      const response = await request(app).get('/albums/abc');
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error');
    });
  });

  describe('POST /albums', () => {
    it('should return 201 and created album with generated ID', async () => {
      const newAlbum = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: 15.99,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(newAlbum);
      
      expect(response.status).toBe(201);
      expect(response.body).toHaveProperty('id');
      expect(response.body.id).toBe(7); // Next available ID
      expect(response.body.title).toBe(newAlbum.title);
      expect(response.body.artist).toBe(newAlbum.artist);
      expect(response.body.price).toBe(newAlbum.price);
    });

    it('should return 400 for missing required fields', async () => {
      const incompleteAlbum = {
        title: 'Test Album'
        // Missing artist, price, image_url
      };

      const response = await request(app)
        .post('/albums')
        .send(incompleteAlbum);
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid price (negative)', async () => {
      const invalidAlbum = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: -5.99,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(invalidAlbum);
      
      expect(response.status).toBe(400);
      expect(response.body.error).toContain('Price');
    });

    it('should return 400 for invalid price (zero)', async () => {
      const invalidAlbum = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: 0,
        image_url: 'https://example.com/image.jpg'
      };

      const response = await request(app)
        .post('/albums')
        .send(invalidAlbum);
      
      expect(response.status).toBe(400);
      expect(response.body.error).toContain('Price');
    });
  });

  describe('PUT /albums/:id', () => {
    it('should return 200 and updated album', async () => {
      const updates = {
        title: 'Updated Title',
        price: 19.99
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updates);
      
      expect(response.status).toBe(200);
      expect(response.body.id).toBe(1);
      expect(response.body.title).toBe('Updated Title');
      expect(response.body.price).toBe(19.99);
      expect(response.body.artist).toBe('Daprize'); // Unchanged
    });

    it('should allow partial updates', async () => {
      const updates = {
        price: 9.99
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updates);
      
      expect(response.status).toBe(200);
      expect(response.body.price).toBe(9.99);
      expect(response.body.title).toBe('You, Me and an App Id'); // Unchanged
    });

    it('should return 404 for non-existent ID', async () => {
      const updates = {
        title: 'Updated Title'
      };

      const response = await request(app)
        .put('/albums/999')
        .send(updates);
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid price', async () => {
      const updates = {
        price: -10
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updates);
      
      expect(response.status).toBe(400);
      expect(response.body.error).toContain('Price');
    });
  });

  describe('DELETE /albums/:id', () => {
    it('should return 204 for successful deletion', async () => {
      const response = await request(app).delete('/albums/1');
      
      expect(response.status).toBe(204);
      expect(response.body).toEqual({});
    });

    it('should actually remove album from store', async () => {
      await request(app).delete('/albums/1');
      
      // Verify album is gone
      const getResponse = await request(app).get('/albums/1');
      expect(getResponse.status).toBe(404);
      
      // Verify total count decreased
      const listResponse = await request(app).get('/albums');
      expect(listResponse.body).toHaveLength(5);
    });

    it('should return 404 for non-existent ID', async () => {
      const response = await request(app).delete('/albums/999');
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error');
    });

    it('should return 400 for invalid ID format', async () => {
      const response = await request(app).delete('/albums/invalid');
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error');
    });
  });

  describe('Integration tests', () => {
    it('should handle complete CRUD workflow', async () => {
      // Create
      const createResponse = await request(app)
        .post('/albums')
        .send({
          title: 'Workflow Album',
          artist: 'Test Artist',
          price: 20.99,
          image_url: 'https://example.com/workflow.jpg'
        });
      
      expect(createResponse.status).toBe(201);
      const newId = createResponse.body.id;

      // Read
      const getResponse = await request(app).get(`/albums/${newId}`);
      expect(getResponse.status).toBe(200);
      expect(getResponse.body.title).toBe('Workflow Album');

      // Update
      const updateResponse = await request(app)
        .put(`/albums/${newId}`)
        .send({ price: 25.99 });
      
      expect(updateResponse.status).toBe(200);
      expect(updateResponse.body.price).toBe(25.99);

      // Delete
      const deleteResponse = await request(app).delete(`/albums/${newId}`);
      expect(deleteResponse.status).toBe(204);

      // Verify deletion
      const verifyResponse = await request(app).get(`/albums/${newId}`);
      expect(verifyResponse.status).toBe(404);
    });
  });
});
