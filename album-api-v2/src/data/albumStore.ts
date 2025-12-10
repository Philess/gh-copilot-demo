import { Album } from '../models/Album';

// In-memory data store with sample albums
let albums: Album[] = [
  {
    id: 1,
    title: 'You, Me and an App Id',
    artist: 'Daprize',
    price: 10.99,
    image_url: 'https://aka.ms/albums-daprlogo'
  },
  {
    id: 2,
    title: 'Seven Revision Army',
    artist: 'The Blue-Green Stripes',
    price: 13.99,
    image_url: 'https://aka.ms/albums-containerappslogo'
  },
  {
    id: 3,
    title: 'Scale It Up',
    artist: 'KEDA Club',
    price: 13.99,
    image_url: 'https://aka.ms/albums-kedalogo'
  },
  {
    id: 4,
    title: 'Lost in Translation',
    artist: 'MegaDNS',
    price: 12.99,
    image_url: 'https://aka.ms/albums-envoylogo'
  },
  {
    id: 5,
    title: 'Lock Down Your Love',
    artist: 'V is for VNET',
    price: 12.99,
    image_url: 'https://aka.ms/albums-vnetlogo'
  },
  {
    id: 6,
    title: "Sweet Container O' Mine",
    artist: 'Guns N Probeses',
    price: 14.99,
    image_url: 'https://aka.ms/albums-containerappslogo'
  }
];

let nextId = 7;

export const albumStore = {
  /**
   * Get all albums
   */
  getAllAlbums(): Album[] {
    return [...albums];
  },

  /**
   * Get album by ID
   */
  getAlbumById(id: number): Album | undefined {
    return albums.find(album => album.id === id);
  },

  /**
   * Add new album
   */
  addAlbum(albumData: Omit<Album, 'id'>): Album {
    const newAlbum: Album = {
      id: nextId++,
      ...albumData
    };
    albums.push(newAlbum);
    return newAlbum;
  },

  /**
   * Update existing album
   */
  updateAlbum(id: number, updates: Partial<Omit<Album, 'id'>>): Album | undefined {
    const index = albums.findIndex(album => album.id === id);
    if (index === -1) {
      return undefined;
    }
    albums[index] = { ...albums[index], ...updates };
    return albums[index];
  },

  /**
   * Delete album by ID
   */
  deleteAlbum(id: number): boolean {
    const index = albums.findIndex(album => album.id === id);
    if (index === -1) {
      return false;
    }
    albums.splice(index, 1);
    return true;
  },

  /**
   * Reset store to initial state (useful for testing)
   */
  reset(): void {
    albums = [
      {
        id: 1,
        title: 'You, Me and an App Id',
        artist: 'Daprize',
        price: 10.99,
        image_url: 'https://aka.ms/albums-daprlogo'
      },
      {
        id: 2,
        title: 'Seven Revision Army',
        artist: 'The Blue-Green Stripes',
        price: 13.99,
        image_url: 'https://aka.ms/albums-containerappslogo'
      },
      {
        id: 3,
        title: 'Scale It Up',
        artist: 'KEDA Club',
        price: 13.99,
        image_url: 'https://aka.ms/albums-kedalogo'
      },
      {
        id: 4,
        title: 'Lost in Translation',
        artist: 'MegaDNS',
        price: 12.99,
        image_url: 'https://aka.ms/albums-envoylogo'
      },
      {
        id: 5,
        title: 'Lock Down Your Love',
        artist: 'V is for VNET',
        price: 12.99,
        image_url: 'https://aka.ms/albums-vnetlogo'
      },
      {
        id: 6,
        title: "Sweet Container O' Mine",
        artist: 'Guns N Probeses',
        price: 14.99,
        image_url: 'https://aka.ms/albums-containerappslogo'
      }
    ];
    nextId = 7;
  }
};
