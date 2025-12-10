import request from 'supertest';
import app from '../src/index';

describe('Album API v2', () => {
  it('GET / should return root message', async () => {
    const res = await request(app).get('/');
    expect(res.status).toBe(200);
    expect(res.text).toBe('Hit the /albums endpoint to retrieve a list of albums!');
  });

  it('GET /albums should return list of albums', async () => {
    const res = await request(app).get('/albums');
    expect(res.status).toBe(200);
    expect(Array.isArray(res.body)).toBe(true);
    expect(res.body.length).toBeGreaterThanOrEqual(6);
    expect(res.body[0]).toHaveProperty('id');
    expect(res.body[0]).toHaveProperty('title');
    expect(res.body[0]).toHaveProperty('artist');
    expect(res.body[0]).toHaveProperty('year');
    expect(res.body[0]).toHaveProperty('genre');
    expect(res.body[0]).toHaveProperty('price');
  });

  it('GET /albums/:id should return single album', async () => {
    const res = await request(app).get('/albums/1');
    expect(res.status).toBe(200);
    expect(res.body.id).toBe(1);
  });

  it('POST /albums should create album', async () => {
    const res = await request(app)
      .post('/albums')
      .send({ title: 'Test', artist: 'Tester', year: 2020, genre: 'Rock', price: 7.5 });
    expect(res.status).toBe(201);
    expect(res.body.id).toBeGreaterThan(0);
  });

  it('PUT /albums/:id should update album', async () => {
    const create = await request(app)
      .post('/albums')
      .send({ title: 'To Update', artist: 'Tester', year: 2021, genre: 'Pop', price: 5 });
    const id = create.body.id;
    const res = await request(app)
      .put(`/albums/${id}`)
      .send({ price: 6 });
    expect(res.status).toBe(200);
    expect(res.body.price).toBe(6);
  });

  it('DELETE /albums/:id should delete album', async () => {
    const create = await request(app)
      .post('/albums')
      .send({ title: 'To Delete', artist: 'Tester', year: 2019, genre: 'Jazz', price: 4 });
    const id = create.body.id;
    const res = await request(app).delete(`/albums/${id}`);
    expect(res.status).toBe(200);
    expect(res.body.id).toBe(id);
  });
});
