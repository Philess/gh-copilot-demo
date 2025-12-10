import { Album } from '../types/album';

export class AlbumStore {
  private albums: Album[] = [
    { id: 1, title: "You, Me and an App Id", artist: "Daprize", price: 10.99, image_url: "https://aka.ms/albums-daprlogo" },
    { id: 2, title: "Seven Revision Army", artist: "The Blue-Green Stripes", price: 13.99, image_url: "https://aka.ms/albums-containerappslogo" },
    { id: 3, title: "Scale It Up", artist: "KEDA Club", price: 13.99, image_url: "https://aka.ms/albums-kedalogo" },
    { id: 4, title: "Lost in Translation", artist: "MegaDNS", price: 12.99, image_url: "https://aka.ms/albums-envoylogo" },
    { id: 5, title: "Lock Down Your Love", artist: "V is for VNET", price: 12.99, image_url: "https://aka.ms/albums-vnetlogo" },
    { id: 6, title: "Sweet Container O' Mine", artist: "Guns N Probeses", price: 14.99, image_url: "https://aka.ms/albums-containerappslogo" }
  ];

  private nextId: number = 7;

  getAll(): Album[] {
    return [...this.albums];
  }

  getById(id: number): Album | undefined {
    return this.albums.find(album => album.id === id);
  }

  create(albumData: Omit<Album, 'id'>): Album {
    const newAlbum: Album = {
      id: this.nextId++,
      ...albumData
    };
    this.albums.push(newAlbum);
    return newAlbum;
  }

  update(id: number, albumData: Partial<Omit<Album, 'id'>>): Album | undefined {
    const index = this.albums.findIndex(album => album.id === id);
    if (index === -1) {
      return undefined;
    }
    this.albums[index] = { ...this.albums[index], ...albumData };
    return this.albums[index];
  }

  delete(id: number): boolean {
    const index = this.albums.findIndex(album => album.id === id);
    if (index === -1) {
      return false;
    }
    this.albums.splice(index, 1);
    return true;
  }
}

export const albumStore = new AlbumStore();
