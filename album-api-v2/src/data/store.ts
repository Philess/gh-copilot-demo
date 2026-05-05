import { Album } from '../types/album'

const SEED: Album[] = [
  {
    id: 1,
    title: "You, Me and an App Id",
    artist: { name: "Daprize", birthdate: "1992-05-14", birthPlace: "Seattle" },
    year: 2020,
    price: 10.99,
    image_url: "https://aka.ms/albums-daprlogo",
  },
  {
    id: 2,
    title: "Seven Revision Army",
    artist: { name: "The Blue-Green Stripes", birthdate: "1988-10-21", birthPlace: "Austin" },
    year: 2021,
    price: 13.99,
    image_url: "https://aka.ms/albums-containerappslogo",
  },
  {
    id: 3,
    title: "Scale It Up",
    artist: { name: "KEDA Club", birthdate: "1990-02-07", birthPlace: "Dublin" },
    year: 2022,
    price: 13.99,
    image_url: "https://aka.ms/albums-kedalogo",
  },
  {
    id: 4,
    title: "Lost in Translation",
    artist: { name: "MegaDNS", birthdate: "1985-07-02", birthPlace: "Amsterdam" },
    year: 2020,
    price: 12.99,
    image_url: "https://aka.ms/albums-envoylogo",
  },
  {
    id: 5,
    title: "Lock Down Your Love",
    artist: { name: "V is for VNET", birthdate: "1991-11-30", birthPlace: "Berlin" },
    year: 2021,
    price: 12.99,
    image_url: "https://aka.ms/albums-vnetlogo",
  },
  {
    id: 6,
    title: "Sweet Container O' Mine",
    artist: { name: "Guns N Probeses", birthdate: "1987-03-19", birthPlace: "Chicago" },
    year: 2022,
    price: 14.99,
    image_url: "https://aka.ms/albums-containerappslogo",
  },
]

function deepClone(albums: Album[]): Album[] {
  return albums.map((a) => ({ ...a, artist: { ...a.artist } }))
}

let albums: Album[] = deepClone(SEED)
let nextId = 7

export function resetStore(): void {
  albums = deepClone(SEED)
  nextId = 7
}

export function getAll(): Album[] {
  return deepClone(albums)
}

export function getById(id: number): Album | undefined {
  const album = albums.find((a) => a.id === id)
  return album ? { ...album, artist: { ...album.artist } } : undefined
}

export function getByYear(year: number): Album[] {
  return deepClone(albums.filter((a) => a.year === year))
}

export function create(data: Omit<Album, 'id'>): Album {
  const album: Album = { id: nextId++, ...data, artist: { ...data.artist } }
  albums.push(album)
  return { ...album, artist: { ...album.artist } }
}

export function update(id: number, data: Omit<Album, 'id'>): Album | null {
  const index = albums.findIndex((a) => a.id === id)
  if (index < 0) return null
  const updated: Album = { id, ...data, artist: { ...data.artist } }
  albums[index] = updated
  return { ...updated, artist: { ...updated.artist } }
}

export function remove(id: number): boolean {
  const index = albums.findIndex((a) => a.id === id)
  if (index < 0) return false
  albums.splice(index, 1)
  return true
}
