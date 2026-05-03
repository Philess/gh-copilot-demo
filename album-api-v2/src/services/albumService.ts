import { Album, CreateAlbumRequest, UpdateAlbumRequest } from '../types/album.js';
import { albums, idCounter } from '../data/albums.js';

/**
 * Album service providing CRUD operations on the in-memory album collection
 */
export class AlbumService {
  /**
   * Get all albums
   * @returns Array of all albums
   */
  getAllAlbums(): Album[] {
    return [...albums];
  }

  /**
   * Get a single album by ID
   * @param id - Album ID
   * @returns Album if found, undefined otherwise
   */
  getAlbumById(id: number): Album | undefined {
    return albums.find(album => album.id === id);
  }

  /**
   * Search albums by year
   * @param year - Release year to filter by
   * @returns Array of albums matching the year
   */
  getAlbumsByYear(year: number): Album[] {
    return albums.filter(album => album.year === year);
  }

  /**
   * Create a new album
   * @param request - Album data
   * @returns Created album with auto-generated ID
   */
  createAlbum(request: CreateAlbumRequest): Album {
    const newAlbum: Album = {
      id: idCounter.nextId,
      title: request.title,
      artist: request.artist,
      price: request.price,
      year: request.year,
      image_url: request.image_url
    };

    albums.push(newAlbum);
    idCounter.nextId++;

    return newAlbum;
  }

  /**
   * Update an existing album
   * @param id - Album ID to update
   * @param request - Updated album data
   * @returns Updated album if found, undefined otherwise
   */
  updateAlbum(id: number, request: UpdateAlbumRequest): Album | undefined {
    const index = albums.findIndex(album => album.id === id);
    
    if (index === -1) {
      return undefined;
    }

    const updatedAlbum: Album = {
      id,
      title: request.title,
      artist: request.artist,
      price: request.price,
      year: request.year,
      image_url: request.image_url
    };

    albums[index] = updatedAlbum;
    return updatedAlbum;
  }

  /**
   * Delete an album
   * @param id - Album ID to delete
   * @returns true if deleted, false if not found
   */
  deleteAlbum(id: number): boolean {
    const index = albums.findIndex(album => album.id === id);
    
    if (index === -1) {
      return false;
    }

    albums.splice(index, 1);
    return true;
  }
}

// Export a singleton instance
export const albumService = new AlbumService();
