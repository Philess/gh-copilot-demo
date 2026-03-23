import request from 'supertest';
import express, { Express } from 'express';
import albumRoutes from './albums';
import { resetAlbums } from '../models/album';

let app: Express;

beforeAll(() => {
  app = express();
  app.use(express.json());
  app.use('/albums', albumRoutes);
});

beforeEach(() => {
  resetAlbums();
});

describe('GET /albums', () => {
  it('should return all 6 albums with HTTP 200', async () => {
    const response = await request(app).get('/albums');

    expect(response.status).toBe(200);
    expect(Array.isArray(response.body)).toBe(true);
    expect(response.body.length).toBe(6);
  });

  it('should return albums with all required properties', async () => {
    const response = await request(app).get('/albums');

    expect(response.status).toBe(200);
    response.body.forEach((album: any) => {
      expect(album).toHaveProperty('id');
      expect(album).toHaveProperty('title');
      expect(album).toHaveProperty('artist');
      expect(album).toHaveProperty('price');
      expect(album).toHaveProperty('image_url');
      expect(typeof album.id).toBe('number');
      expect(typeof album.title).toBe('string');
      expect(typeof album.artist).toBe('string');
      expect(typeof album.price).toBe('number');
      expect(typeof album.image_url).toBe('string');
    });
  });

  it('should return albums in expected order', async () => {
    const response = await request(app).get('/albums');

    expect(response.status).toBe(200);
    expect(response.body[0].id).toBe(1);
    expect(response.body[0].title).toBe('You, Me and an App Id');
    expect(response.body[1].id).toBe(2);
    expect(response.body[5].id).toBe(6);
  });
});

describe('GET /albums/:id', () => {
  it('should return specific album with HTTP 200 when ID exists', async () => {
    const response = await request(app).get('/albums/1');

    expect(response.status).toBe(200);
    expect(response.body.id).toBe(1);
    expect(response.body.title).toBe('You, Me and an App Id');
  });

  it('should return all album properties correctly', async () => {
    const response = await request(app).get('/albums/1');

    expect(response.status).toBe(200);
    expect(response.body).toEqual({
      id: 1,
      title: 'You, Me and an App Id',
      artist: 'Daprize',
      price: 10.99,
      image_url: 'https://aka.ms/albums-daprlogo'
    });
  });

  it('should return HTTP 404 with error message when ID does not exist', async () => {
    const response = await request(app).get('/albums/999');

    expect(response.status).toBe(404);
    expect(response.body).toEqual({ error: 'Album not found' });
  });

  it('should return HTTP 400 when ID is not a valid number', async () => {
    const response = await request(app).get('/albums/invalid');

    expect(response.status).toBe(400);
    expect(response.body).toEqual({ error: 'Invalid album ID' });
  });

  it('should work with different valid IDs', async () => {
    const response = await request(app).get('/albums/3');

    expect(response.status).toBe(200);
    expect(response.body.id).toBe(3);
    expect(response.body.title).toBe('Scale It Up');
  });
});

describe('POST /albums', () => {
  it('should create new album with HTTP 201', async () => {
    const newAlbum = {
      title: 'New Album',
      artist: 'New Artist',
      price: 11.99,
      image_url: 'https://example.com/image.jpg'
    };

    const response = await request(app).post('/albums').send(newAlbum);

    expect(response.status).toBe(201);
    expect(response.body).toMatchObject(newAlbum);
    expect(response.body.id).toBeDefined();
  });

  it('should auto-increment ID (next ID after existing highest)', async () => {
    const newAlbum = {
      title: 'New Album',
      artist: 'New Artist',
      price: 11.99,
      image_url: 'https://example.com/image.jpg'
    };

    const response = await request(app).post('/albums').send(newAlbum);

    expect(response.status).toBe(201);
    expect(response.body.id).toBe(7);
  });

  it('should return created album with all properties', async () => {
    const newAlbum = {
      title: 'Test Album',
      artist: 'Test Artist',
      price: 12.99,
      image_url: 'https://example.com/test.jpg'
    };

    const response = await request(app).post('/albums').send(newAlbum);

    expect(response.status).toBe(201);
    expect(response.body).toHaveProperty('id');
    expect(response.body).toHaveProperty('title');
    expect(response.body).toHaveProperty('artist');
    expect(response.body).toHaveProperty('price');
    expect(response.body).toHaveProperty('image_url');
  });

  it('should persist album to in-memory store (verify with subsequent GET)', async () => {
    const newAlbum = {
      title: 'Persistent Album',
      artist: 'Persistent Artist',
      price: 15.99,
      image_url: 'https://example.com/persistent.jpg'
    };

    const createResponse = await request(app).post('/albums').send(newAlbum);
    const createdId = createResponse.body.id;

    const getResponse = await request(app).get(`/albums/${createdId}`);

    expect(getResponse.status).toBe(200);
    expect(getResponse.body.title).toBe('Persistent Album');
  });

  it('should return HTTP 400 if title is missing', async () => {
    const response = await request(app).post('/albums').send({
      artist: 'Artist',
      price: 12.99,
      image_url: 'https://example.com/image.jpg'
    });

    expect(response.status).toBe(400);
    expect(response.body.error).toContain('title');
  });

  it('should return HTTP 400 if artist is missing', async () => {
    const response = await request(app).post('/albums').send({
      title: 'Title',
      price: 12.99,
      image_url: 'https://example.com/image.jpg'
    });

    expect(response.status).toBe(400);
    expect(response.body.error).toContain('artist');
  });

  it('should return HTTP 400 if price is missing', async () => {
    const response = await request(app).post('/albums').send({
      title: 'Title',
      artist: 'Artist',
      image_url: 'https://example.com/image.jpg'
    });

    expect(response.status).toBe(400);
    expect(response.body.error).toContain('price');
  });

  it('should return HTTP 400 if image_url is missing', async () => {
    const response = await request(app).post('/albums').send({
      title: 'Title',
      artist: 'Artist',
      price: 12.99
    });

    expect(response.status).toBe(400);
    expect(response.body.error).toContain('image_url');
  });

  it('should return HTTP 400 if title is empty string', async () => {
    const response = await request(app).post('/albums').send({
      title: '',
      artist: 'Artist',
      price: 12.99,
      image_url: 'https://example.com/image.jpg'
    });

    expect(response.status).toBe(400);
  });

  it('should return HTTP 400 if price is negative', async () => {
    const response = await request(app).post('/albums').send({
      title: 'Title',
      artist: 'Artist',
      price: -5.99,
      image_url: 'https://example.com/image.jpg'
    });

    expect(response.status).toBe(400);
  });
});

describe('PUT /albums/:id', () => {
  it('should update existing album with HTTP 200', async () => {
    const updatedAlbum = {
      title: 'Updated Title',
      artist: 'Updated Artist',
      price: 19.99,
      image_url: 'https://example.com/updated.jpg'
    };

    const response = await request(app).put('/albums/1').send(updatedAlbum);

    expect(response.status).toBe(200);
    expect(response.body).toMatchObject(updatedAlbum);
  });

  it('should preserve ID during update', async () => {
    const updatedAlbum = {
      title: 'Updated Title',
      artist: 'Updated Artist',
      price: 19.99,
      image_url: 'https://example.com/updated.jpg'
    };

    const response = await request(app).put('/albums/1').send(updatedAlbum);

    expect(response.status).toBe(200);
    expect(response.body.id).toBe(1);
  });

  it('should return HTTP 404 if album ID does not exist', async () => {
    const updatedAlbum = {
      title: 'Updated Title',
      artist: 'Updated Artist',
      price: 19.99,
      image_url: 'https://example.com/updated.jpg'
    };

    const response = await request(app).put('/albums/999').send(updatedAlbum);

    expect(response.status).toBe(404);
    expect(response.body).toEqual({ error: 'Album not found' });
  });

  it('should return HTTP 400 if required fields are missing', async () => {
    const response = await request(app).put('/albums/1').send({
      title: 'Updated Title'
    });

    expect(response.status).toBe(400);
  });

  it('should persist changes to in-memory store', async () => {
    const updatedAlbum = {
      title: 'Permanently Updated',
      artist: 'Updated Artist',
      price: 19.99,
      image_url: 'https://example.com/updated.jpg'
    };

    const updateResponse = await request(app).put('/albums/1').send(updatedAlbum);
    const getResponse = await request(app).get('/albums/1');

    expect(updateResponse.status).toBe(200);
    expect(getResponse.status).toBe(200);
    expect(getResponse.body.title).toBe('Permanently Updated');
  });

  it('should return HTTP 400 if ID in URL is invalid', async () => {
    const updatedAlbum = {
      title: 'Updated Title',
      artist: 'Updated Artist',
      price: 19.99,
      image_url: 'https://example.com/updated.jpg'
    };

    const response = await request(app).put('/albums/invalid').send(updatedAlbum);

    expect(response.status).toBe(400);
  });
});

describe('DELETE /albums/:id', () => {
  it('should delete album with HTTP 204 when ID exists', async () => {
    const response = await request(app).delete('/albums/1');

    expect(response.status).toBe(204);
  });

  it('should remove album from store (verify with subsequent GET)', async () => {
    await request(app).delete('/albums/1');
    const response = await request(app).get('/albums/1');

    expect(response.status).toBe(404);
  });

  it('should return HTTP 404 if album ID does not exist', async () => {
    const response = await request(app).delete('/albums/999');

    expect(response.status).toBe(404);
    expect(response.body).toEqual({ error: 'Album not found' });
  });

  it('should return HTTP 400 if ID in URL is invalid', async () => {
    const response = await request(app).delete('/albums/invalid');

    expect(response.status).toBe(400);
  });

  it('should reduce total album count after deletion', async () => {
    const beforeResponse = await request(app).get('/albums');
    const countBefore = beforeResponse.body.length;

    await request(app).delete('/albums/1');

    const afterResponse = await request(app).get('/albums');
    const countAfter = afterResponse.body.length;

    expect(countAfter).toBe(countBefore - 1);
  });

  it('should not affect other albums when deleting one', async () => {
    await request(app).delete('/albums/2');

    const response = await request(app).get('/albums/1');

    expect(response.status).toBe(200);
    expect(response.body.id).toBe(1);
    expect(response.body.title).toBe('You, Me and an App Id');
  });
});

describe('Integration Tests', () => {
  it('should handle multiple creates and verify they are all retrievable', async () => {
    const album1 = {
      title: 'Album 1',
      artist: 'Artist 1',
      price: 10.99,
      image_url: 'https://example.com/1.jpg'
    };

    const album2 = {
      title: 'Album 2',
      artist: 'Artist 2',
      price: 12.99,
      image_url: 'https://example.com/2.jpg'
    };

    const create1 = await request(app).post('/albums').send(album1);
    const create2 = await request(app).post('/albums').send(album2);

    const get1 = await request(app).get(`/albums/${create1.body.id}`);
    const get2 = await request(app).get(`/albums/${create2.body.id}`);

    expect(get1.status).toBe(200);
    expect(get2.status).toBe(200);
    expect(get1.body.title).toBe('Album 1');
    expect(get2.body.title).toBe('Album 2');
  });

  it('should handle create, update, and delete workflow', async () => {
    const newAlbum = {
      title: 'Workflow Album',
      artist: 'Workflow Artist',
      price: 13.99,
      image_url: 'https://example.com/workflow.jpg'
    };

    const createResponse = await request(app).post('/albums').send(newAlbum);
    const albumId = createResponse.body.id;

    const updatedAlbum = {
      title: 'Updated Workflow',
      artist: 'Updated Workflow Artist',
      price: 14.99,
      image_url: 'https://example.com/updated-workflow.jpg'
    };

    const updateResponse = await request(app).put(`/albums/${albumId}`).send(updatedAlbum);
    expect(updateResponse.status).toBe(200);

    const deleteResponse = await request(app).delete(`/albums/${albumId}`);
    expect(deleteResponse.status).toBe(204);

    const getResponse = await request(app).get(`/albums/${albumId}`);
    expect(getResponse.status).toBe(404);
  });
});
