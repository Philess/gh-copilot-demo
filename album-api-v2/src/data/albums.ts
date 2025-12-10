import { Album } from '../models/album.js';

// Sample data matching the original .NET albums-api
export const albums: Album[] = [
  {
    id: 1,
    title: "You, Me and an App Id",
    artist: "Daprize",
    price: 10.99,
    image_url: "https://aka.ms/albums-daprlogo"
  },
  {
    id: 2,
    title: "Seven Revision Army",
    artist: "The Blue-Green Stripes",
    price: 13.99,
    image_url: "https://aka.ms/albums-containerappslogo"
  },
  {
    id: 3,
    title: "Scale It Up",
    artist: "KEDA Club",
    price: 13.99,
    image_url: "https://aka.ms/albums-kedalogo"
  },
  {
    id: 4,
    title: "Lost in Translation",
    artist: "MegaDNS",
    price: 12.99,
    image_url: "https://aka.ms/albums-envoylogo"
  },
  {
    id: 5,
    title: "Lock Down Your Love",
    artist: "V is for VNET",
    price: 12.99,
    image_url: "https://aka.ms/albums-vnetlogo"
  },
  {
    id: 6,
    title: "Sweet Container O' Mine",
    artist: "Guns N Probeses",
    price: 14.99,
    image_url: "https://aka.ms/albums-containerappslogo"
  }
];

// Helper to get next available ID
export function getNextId(): number {
  if (albums.length === 0) return 1;
  return Math.max(...albums.map(a => a.id)) + 1;
}

// Helper to reset albums to initial state (useful for testing)
export function resetAlbums(): void {
  albums.length = 0;
  albums.push(
    { id: 1, title: "You, Me and an App Id", artist: "Daprize", price: 10.99, image_url: "https://aka.ms/albums-daprlogo" },
    { id: 2, title: "Seven Revision Army", artist: "The Blue-Green Stripes", price: 13.99, image_url: "https://aka.ms/albums-containerappslogo" },
    { id: 3, title: "Scale It Up", artist: "KEDA Club", price: 13.99, image_url: "https://aka.ms/albums-kedalogo" },
    { id: 4, title: "Lost in Translation", artist: "MegaDNS", price: 12.99, image_url: "https://aka.ms/albums-envoylogo" },
    { id: 5, title: "Lock Down Your Love", artist: "V is for VNET", price: 12.99, image_url: "https://aka.ms/albums-vnetlogo" },
    { id: 6, title: "Sweet Container O' Mine", artist: "Guns N Probeses", price: 14.99, image_url: "https://aka.ms/albums-containerappslogo" }
  );
}
