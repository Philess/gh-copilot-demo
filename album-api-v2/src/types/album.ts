/**
 * Album interface matching the Vue.js app expectations
 * The artist field is a string (artist name only) to match the frontend TypeScript definition
 */
export interface Album {
  id: number;
  title: string;
  artist: string;
  price: number;
  year: number;
  image_url: string;
}

/**
 * Request DTO for creating a new album
 */
export interface CreateAlbumRequest {
  title: string;
  artist: string;
  price: number;
  year: number;
  image_url: string;
}

/**
 * Request DTO for updating an existing album
 */
export interface UpdateAlbumRequest {
  title: string;
  artist: string;
  price: number;
  year: number;
  image_url: string;
}
