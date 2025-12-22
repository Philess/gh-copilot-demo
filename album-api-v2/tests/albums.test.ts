import request from 'supertest';
import app from '../src/app';

describe('Albums API', () => {
  it('GET /albums returns all albums', async () => {
    const res = await request(app).get('/albums');
    expect(res.status).toBe(200);
    expect(Array.isArray(res.body)).toBe(true);
    expect(res.body.length).toBeGreaterThanOrEqual(6);
  });

  it('GET /albums?year=2020 filters by year', async () => {
    const res = await request(app).get('/albums').query({ year: 2020 });
    expect(res.status).toBe(200);
    expect(res.body.every((a: any) => a.Year === 2020)).toBe(true);
  });

  it('GET /albums/:id returns single album', async () => {
    const res = await request(app).get('/albums/1');
    expect(res.status).toBe(200);
    expect(res.body.Id).toBe(1);
  });

  it('GET /albums/:id returns 404 for missing id', async () => {
    const res = await request(app).get('/albums/9999');
    expect(res.status).toBe(404);
  });

  it('POST /albums creates an album', async () => {
    const newAlbum = {
      Title: 'New Test',
      Artist: { Name: 'Tester', Birthdate: '2000-01-01T00:00:00', BirthPlace: 'Nowhere' },
      Year: 2025,
      Price: 9.99,
      Image_url: ''
    };
    const res = await request(app).post('/albums').send(newAlbum);
    expect(res.status).toBe(201);
    expect(res.body.Title).toBe('New Test');
    expect(res.body.Id).toBeDefined();
  });

  it('PUT /albums/:id updates an album', async () => {
    const update = { Title: 'Updated Title' };
    const res = await request(app).put('/albums/1').send(update);
    expect(res.status).toBe(200);
    expect(res.body.Title).toBe('Updated Title');
  });

  it('DELETE /albums/:id deletes an album', async () => {
    // create a new album to delete
    const create = await request(app).post('/albums').send({
      Title: 'To Delete',
      Artist: { Name: 'X', Birthdate: '2000-01-01T00:00:00', BirthPlace: 'Y' },
      Year: 2025,
      Price: 1,
      Image_url: ''
    });
    const id = create.body.Id;
    const del = await request(app).delete(`/albums/${id}`);
    expect(del.status).toBe(204);
    const get = await request(app).get(`/albums/${id}`);
    expect(get.status).toBe(404);
  });
});
