import request from 'supertest';
import app from '../index';

describe('Album API', () => {
  describe('GET /albums', () => {
    it('should return all albums', async () => {
      const response = await request(app).get('/albums');
      
      expect(response.status).toBe(200);
      expect(Array.isArray(response.body)).toBe(true);
      expect(response.body.length).toBeGreaterThan(0);
      expect(response.body[0]).toHaveProperty('id');
      expect(response.body[0]).toHaveProperty('title');
      expect(response.body[0]).toHaveProperty('artist');
      expect(response.body[0]).toHaveProperty('price');
      expect(response.body[0]).toHaveProperty('image_url');
    });
  });

  describe('GET /albums/:id', () => {
    it('should return an album by id', async () => {
      const response = await request(app).get('/albums/1');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('id', 1);
      expect(response.body).toHaveProperty('title');
      expect(response.body).toHaveProperty('artist');
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).get('/albums/9999');
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error', 'Album not found');
    });

    it('should return 400 for invalid album id', async () => {
      const response = await request(app).get('/albums/invalid');
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error', 'Invalid album ID');
    });
  });

  describe('POST /albums', () => {
    it('should create a new album', async () => {
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
      expect(response.body.title).toBe(newAlbum.title);
      expect(response.body.artist).toBe(newAlbum.artist);
      expect(response.body.price).toBe(newAlbum.price);
      expect(response.body.image_url).toBe(newAlbum.image_url);
    });

    it('should return 400 for missing required fields', async () => {
      const invalidAlbum = {
        title: 'Test Album'
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
      expect(response.body).toHaveProperty('error', 'Price must be a positive number');
    });
  });

  describe('PUT /albums/:id', () => {
    it('should update an existing album', async () => {
      const updates = {
        title: 'Updated Title',
        price: 20.99
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updates);
      
      expect(response.status).toBe(200);
      expect(response.body.title).toBe(updates.title);
      expect(response.body.price).toBe(updates.price);
      expect(response.body).toHaveProperty('artist');
    });

    it('should return 404 for non-existent album', async () => {
      const updates = {
        title: 'Updated Title'
      };

      const response = await request(app)
        .put('/albums/9999')
        .send(updates);
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error', 'Album not found');
    });

    it('should return 400 for invalid album id', async () => {
      const updates = {
        title: 'Updated Title'
      };

      const response = await request(app)
        .put('/albums/invalid')
        .send(updates);
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error', 'Invalid album ID');
    });

    it('should return 400 for invalid price in update', async () => {
      const updates = {
        price: -5
      };

      const response = await request(app)
        .put('/albums/1')
        .send(updates);
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error', 'Price must be a positive number');
    });
  });

  describe('DELETE /albums/:id', () => {
    it('should delete an existing album', async () => {
      // First create an album to delete
      const newAlbum = {
        title: 'Album to Delete',
        artist: 'Test Artist',
        price: 10.99,
        image_url: 'https://example.com/image.jpg'
      };

      const createResponse = await request(app)
        .post('/albums')
        .send(newAlbum);
      
      const albumId = createResponse.body.id;

      const deleteResponse = await request(app).delete(`/albums/${albumId}`);
      
      expect(deleteResponse.status).toBe(204);

      // Verify album is deleted
      const getResponse = await request(app).get(`/albums/${albumId}`);
      expect(getResponse.status).toBe(404);
    });

    it('should return 404 for non-existent album', async () => {
      const response = await request(app).delete('/albums/9999');
      
      expect(response.status).toBe(404);
      expect(response.body).toHaveProperty('error', 'Album not found');
    });

    it('should return 400 for invalid album id', async () => {
      const response = await request(app).delete('/albums/invalid');
      
      expect(response.status).toBe(400);
      expect(response.body).toHaveProperty('error', 'Invalid album ID');
    });
  });

  describe('GET /', () => {
    it('should return health check message', async () => {
      const response = await request(app).get('/');
      
      expect(response.status).toBe(200);
      expect(response.body).toHaveProperty('message', 'Album API v2 is running');
    });
  });
});
