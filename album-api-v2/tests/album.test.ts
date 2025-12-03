import request from 'supertest';
import express from 'express';
import { albums, Album } from '../src/album';
import api from '../src/index';

// Setup Express app for testing
const app = express();
app.use(express.json());
app.use('/', api);

describe('Album API v2', () => {
  it('GET /albums returns all albums', async () => {
    const res = await request(app).get('/albums');
    expect(res.status).toBe(200);
    expect(res.body.length).toBe(albums.length);
  });

  it('GET /albums/:id returns album by id', async () => {
    const res = await request(app).get('/albums/1');
    expect(res.status).toBe(200);
    expect(res.body.id).toBe(1);
  });

  it('POST /albums creates a new album', async () => {
    const newAlbum = { title: 'Test', artist: 'Tester', year: 2025 };
    const res = await request(app).post('/albums').send(newAlbum);
    expect(res.status).toBe(201);
    expect(res.body.title).toBe('Test');
  });

  it('PUT /albums/:id updates an album', async () => {
    const update = { title: 'Updated', artist: 'Updater', year: 2024 };
    const res = await request(app).put('/albums/1').send(update);
    expect(res.status).toBe(204);
    expect(albums[0].title).toBe('Updated');
  });

  it('DELETE /albums/:id deletes an album', async () => {
    const res = await request(app).delete('/albums/2');
    expect(res.status).toBe(204);
    expect(albums.find(a => a.id === 2)).toBeUndefined();
  });

  it('GET /albums/search?year=1980 returns correct albums', async () => {
    const res = await request(app).get('/albums/search?year=1980');
    expect(res.status).toBe(200);
    expect(res.body[0].year).toBe(1980);
  });
});
