import request from 'supertest';
import app from '../src/index';

describe('Album API v2', () => {
  it('GET / returns greeting text', async () => {
    const res = await request(app).get('/');
    expect(res.status).toBe(200);
    expect(res.text).toContain('Hit the /albums endpoint');
  });

  it('GET /albums returns the seeded list', async () => {
    const res = await request(app).get('/albums');
    expect(res.status).toBe(200);
    expect(Array.isArray(res.body)).toBe(true);
    expect(res.body.length).toBe(6);
    expect(res.body[0]).toMatchObject({
      id: 1,
      title: 'You, Me and an App Id',
      artist: 'Daprize',
      price: 10.99,
      image_url: 'https://aka.ms/albums-daprlogo',
    });
  });

  it('GET /albums/:id returns one album', async () => {
    const res = await request(app).get('/albums/2');
    expect(res.status).toBe(200);
    expect(res.body).toMatchObject({ id: 2, title: 'Seven Revision Army' });
  });

  it('POST /albums creates a new album', async () => {
    const album = {
      title: 'New Jam',
      artist: 'Testers',
      price: 9.99,
      image_url: 'https://example.com/image.png',
    };
    const res = await request(app).post('/albums').send(album);
    expect(res.status).toBe(201);
    expect(res.body).toMatchObject(album);
    expect(typeof res.body.id).toBe('number');
  });

  it('PUT /albums/:id updates an album', async () => {
    const updated = {
      title: 'Updated Jam',
      artist: 'Testers',
      price: 11.99,
      image_url: 'https://example.com/image2.png',
    };
    const res = await request(app).put('/albums/1').send(updated);
    expect(res.status).toBe(200);
    expect(res.body).toMatchObject({ id: 1, ...updated });
  });

  it('DELETE /albums/:id removes an album', async () => {
    const res = await request(app).delete('/albums/6');
    expect(res.status).toBe(204);
    const list = await request(app).get('/albums');
    expect(list.body.find((a: any) => a.id === 6)).toBeUndefined();
  });

  it('POST /albums validates payload', async () => {
    const res = await request(app).post('/albums').send({ title: '', artist: '', price: 'oops' });
    expect(res.status).toBe(400);
    expect(res.body.errors).toBeDefined();
  });
});
