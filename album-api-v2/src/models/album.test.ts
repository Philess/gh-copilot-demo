import { AlbumStore, Album } from '../models/album';

describe('AlbumStore', () => {
  let store: AlbumStore;

  beforeEach(() => {
    store = new AlbumStore();
  });

  describe('getAll', () => {
    it('should return all albums', () => {
      const albums = store.getAll();
      expect(albums).toHaveLength(6);
      expect(albums[0].title).toBe('You, Me and an App Id');
    });

    it('should return a copy of the albums array', () => {
      const albums1 = store.getAll();
      const albums2 = store.getAll();
      expect(albums1).not.toBe(albums2);
      expect(albums1).toEqual(albums2);
    });
  });

  describe('getById', () => {
    it('should return an album by id', () => {
      const album = store.getById(1);
      expect(album).toBeDefined();
      expect(album?.title).toBe('You, Me and an App Id');
      expect(album?.artist).toBe('Daprize');
    });

    it('should return undefined for non-existent id', () => {
      const album = store.getById(999);
      expect(album).toBeUndefined();
    });
  });

  describe('create', () => {
    it('should create a new album', () => {
      const newAlbum = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: 9.99,
        image_url: 'https://example.com/image.jpg'
      };

      const created = store.create(newAlbum);
      expect(created.id).toBe(7);
      expect(created.title).toBe('Test Album');
      expect(store.getAll()).toHaveLength(7);
    });

    it('should auto-increment id for multiple creates', () => {
      const album1 = store.create({
        title: 'Album 1',
        artist: 'Artist 1',
        price: 10,
        image_url: 'url1'
      });
      const album2 = store.create({
        title: 'Album 2',
        artist: 'Artist 2',
        price: 15,
        image_url: 'url2'
      });

      expect(album1.id).toBe(7);
      expect(album2.id).toBe(8);
    });
  });

  describe('update', () => {
    it('should update an existing album', () => {
      const updated = store.update(1, {
        title: 'Updated Title',
        price: 19.99
      });

      expect(updated).toBeDefined();
      expect(updated?.title).toBe('Updated Title');
      expect(updated?.price).toBe(19.99);
      expect(updated?.artist).toBe('Daprize'); // unchanged
    });

    it('should return undefined for non-existent id', () => {
      const updated = store.update(999, { title: 'New Title' });
      expect(updated).toBeUndefined();
    });

    it('should allow partial updates', () => {
      const updated = store.update(1, { price: 99.99 });
      expect(updated?.price).toBe(99.99);
      expect(updated?.title).toBe('You, Me and an App Id');
    });
  });

  describe('delete', () => {
    it('should delete an existing album', () => {
      const result = store.delete(1);
      expect(result).toBe(true);
      expect(store.getAll()).toHaveLength(5);
      expect(store.getById(1)).toBeUndefined();
    });

    it('should return false for non-existent id', () => {
      const result = store.delete(999);
      expect(result).toBe(false);
      expect(store.getAll()).toHaveLength(6);
    });
  });
});
