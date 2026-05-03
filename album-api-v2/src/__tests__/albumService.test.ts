import { AlbumService } from '../services/albumService.js';
import { albums, idCounter } from '../data/albums.js';
import { CreateAlbumRequest, UpdateAlbumRequest } from '../types/album.js';

describe('AlbumService', () => {
  let albumService: AlbumService;
  let initialAlbums: any[];
  let initialNextId: number;

  beforeEach(() => {
    albumService = new AlbumService();
    // Store initial state
    initialAlbums = [...albums];
    initialNextId = idCounter.nextId;
  });

  afterEach(() => {
    // Restore initial state after each test
    albums.length = 0;
    albums.push(...initialAlbums);
    idCounter.nextId = initialNextId;
  });

  describe('getAllAlbums', () => {
    it('should return all albums', () => {
      const result = albumService.getAllAlbums();
      expect(result).toHaveLength(6);
      expect(result[0].title).toBe('You, Me and an App Id');
    });

    it('should return a copy of the albums array', () => {
      const result = albumService.getAllAlbums();
      expect(result).not.toBe(albums);
    });
  });

  describe('getAlbumById', () => {
    it('should return an album when it exists', () => {
      const result = albumService.getAlbumById(1);
      expect(result).toBeDefined();
      expect(result?.id).toBe(1);
      expect(result?.title).toBe('You, Me and an App Id');
      expect(result?.artist).toBe('Daprize');
    });

    it('should return undefined when album does not exist', () => {
      const result = albumService.getAlbumById(999);
      expect(result).toBeUndefined();
    });
  });

  describe('getAlbumsByYear', () => {
    it('should return albums from the specified year', () => {
      const result = albumService.getAlbumsByYear(2021);
      expect(result).toHaveLength(4);
      result.forEach(album => {
        expect(album.year).toBe(2021);
      });
    });

    it('should return empty array when no albums match the year', () => {
      const result = albumService.getAlbumsByYear(1999);
      expect(result).toHaveLength(0);
    });

    it('should return albums from 2020', () => {
      const result = albumService.getAlbumsByYear(2020);
      expect(result).toHaveLength(2);
      expect(result[0].title).toBe('Seven Revision Army');
      expect(result[1].title).toBe('Lost in Translation');
    });
  });

  describe('createAlbum', () => {
    it('should create a new album with auto-generated ID', () => {
      const request: CreateAlbumRequest = {
        title: 'New Album',
        artist: 'New Artist',
        price: 15.99,
        year: 2023,
        image_url: 'https://example.com/image.jpg'
      };

      const result = albumService.createAlbum(request);
      
      expect(result.id).toBe(7);
      expect(result.title).toBe('New Album');
      expect(result.artist).toBe('New Artist');
      expect(result.price).toBe(15.99);
      expect(result.year).toBe(2023);
      expect(result.image_url).toBe('https://example.com/image.jpg');
    });

    it('should increment the next ID after creating an album', () => {
      const request: CreateAlbumRequest = {
        title: 'Album 1',
        artist: 'Artist 1',
        price: 10.0,
        year: 2023,
        image_url: 'https://example.com/1.jpg'
      };

      const album1 = albumService.createAlbum(request);
      expect(album1.id).toBe(7);
      
      const album2 = albumService.createAlbum(request);
      expect(album2.id).toBe(8);
    });

    it('should add the album to the collection', () => {
      const initialLength = albumService.getAllAlbums().length;
      
      const request: CreateAlbumRequest = {
        title: 'Test Album',
        artist: 'Test Artist',
        price: 12.99,
        year: 2023,
        image_url: 'https://example.com/test.jpg'
      };

      albumService.createAlbum(request);
      
      const newLength = albumService.getAllAlbums().length;
      expect(newLength).toBe(initialLength + 1);
    });
  });

  describe('updateAlbum', () => {
    it('should update an existing album', () => {
      const request: UpdateAlbumRequest = {
        title: 'Updated Title',
        artist: 'Updated Artist',
        price: 99.99,
        year: 2024,
        image_url: 'https://example.com/updated.jpg'
      };

      const result = albumService.updateAlbum(1, request);
      
      expect(result).toBeDefined();
      expect(result?.id).toBe(1);
      expect(result?.title).toBe('Updated Title');
      expect(result?.artist).toBe('Updated Artist');
      expect(result?.price).toBe(99.99);
      expect(result?.year).toBe(2024);
    });

    it('should return undefined when album does not exist', () => {
      const request: UpdateAlbumRequest = {
        title: 'Test',
        artist: 'Test',
        price: 10.0,
        year: 2023,
        image_url: 'https://example.com/test.jpg'
      };

      const result = albumService.updateAlbum(999, request);
      expect(result).toBeUndefined();
    });

    it('should preserve the album ID after update', () => {
      const request: UpdateAlbumRequest = {
        title: 'New Title',
        artist: 'New Artist',
        price: 20.0,
        year: 2023,
        image_url: 'https://example.com/new.jpg'
      };

      albumService.updateAlbum(3, request);
      const updated = albumService.getAlbumById(3);
      
      expect(updated?.id).toBe(3);
      expect(updated?.title).toBe('New Title');
    });
  });

  describe('deleteAlbum', () => {
    it('should delete an existing album', () => {
      const initialLength = albumService.getAllAlbums().length;
      const result = albumService.deleteAlbum(1);
      
      expect(result).toBe(true);
      expect(albumService.getAllAlbums().length).toBe(initialLength - 1);
      expect(albumService.getAlbumById(1)).toBeUndefined();
    });

    it('should return false when album does not exist', () => {
      const result = albumService.deleteAlbum(999);
      expect(result).toBe(false);
    });

    it('should not affect other albums when deleting', () => {
      const album2 = albumService.getAlbumById(2);
      const album3 = albumService.getAlbumById(3);
      
      albumService.deleteAlbum(1);
      
      expect(albumService.getAlbumById(2)).toEqual(album2);
      expect(albumService.getAlbumById(3)).toEqual(album3);
    });
  });
});
