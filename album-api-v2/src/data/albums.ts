import { Album } from '../types/album.js';

/**
 * In-memory album collection with seed data matching the .NET API
 * Data includes 6 albums with exact same content from the original albums-api
 */
export let albums: Album[] = [
  {
    id: 1,
    title: "You, Me and an App Id",
    artist: "Daprize",
    price: 10.99,
    year: 2021,
    image_url: "https://aka.ms/albums-daprlogo"
  },
  {
    id: 2,
    title: "Seven Revision Army",
    artist: "The Blue-Green Stripes",
    price: 13.99,
    year: 2020,
    image_url: "https://aka.ms/albums-containerappslogo"
  },
  {
    id: 3,
    title: "Scale It Up",
    artist: "KEDA Club",
    price: 13.99,
    year: 2021,
    image_url: "https://aka.ms/albums-kedalogo"
  },
  {
    id: 4,
    title: "Lost in Translation",
    artist: "MegaDNS",
    price: 12.99,
    year: 2020,
    image_url: "https://aka.ms/albums-envoylogo"
  },
  {
    id: 5,
    title: "Lock Down Your Love",
    artist: "V is for VNET",
    price: 12.99,
    year: 2021,
    image_url: "https://aka.ms/albums-vnetlogo"
  },
  {
    id: 6,
    title: "Sweet Container O' Mine",
    artist: "Guns N Probeses",
    price: 14.99,
    year: 2021,
    image_url: "https://aka.ms/albums-containerappslogo"
  }
];

/**
 * Counter object for auto-incrementing album IDs (starts at 7 after the 6 seed albums)
 * Using an object to maintain reference across imports
 */
export const idCounter = {
  nextId: 7
};
