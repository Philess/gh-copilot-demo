import { Album, AlbumInput } from '../models/album'

let albums: Album[] = [
  { id: 1, title: 'You, Me and an App Id', artist: { name: 'Daprize', birthdate: '1988-04-12', birthPlace: 'Seattle' }, year: 2021, price: 10.99, image_url: 'https://aka.ms/albums-daprlogo' },
  { id: 2, title: 'Seven Revision Army', artist: { name: 'The Blue-Green Stripes', birthdate: '1985-07-23', birthPlace: 'Detroit' }, year: 2020, price: 13.99, image_url: 'https://aka.ms/albums-containerappslogo' },
  { id: 3, title: 'Scale It Up', artist: { name: 'KEDA Club', birthdate: '1990-02-08', birthPlace: 'Chicago' }, year: 2019, price: 13.99, image_url: 'https://aka.ms/albums-kedalogo' },
  { id: 4, title: 'Lost in Translation', artist: { name: 'MegaDNS', birthdate: '1982-11-19', birthPlace: 'London' }, year: 2018, price: 12.99, image_url: 'https://aka.ms/albums-envoylogo' },
  { id: 5, title: 'Lock Down Your Love', artist: { name: 'V is for VNET', birthdate: '1987-06-04', birthPlace: 'Toronto' }, year: 2017, price: 12.99, image_url: 'https://aka.ms/albums-vnetlogo' },
  { id: 6, title: "Sweet Container O' Mine", artist: { name: 'Guns N Probeses', birthdate: '1981-09-15', birthPlace: 'Los Angeles' }, year: 2016, price: 14.99, image_url: 'https://aka.ms/albums-containerappslogo' }
]

export function getAll(): Album[] {
  return [...albums]
}

export function getById(id: number): Album | undefined {
  return albums.find(album => album.id === id)
}

export function getByYear(year: number): Album[] {
  return albums.filter(album => album.year === year)
}

export function create(input: AlbumInput): Album {
  const id = albums.length === 0 ? 1 : Math.max(...albums.map(a => a.id)) + 1
  const createdAlbum: Album = { ...input, id }
  albums.push(createdAlbum)
  return createdAlbum
}

export function update(id: number, input: AlbumInput): Album | undefined {
  const index = albums.findIndex(album => album.id === id)
  if (index < 0) {
    return undefined
  }

  const updatedAlbum: Album = { ...input, id }
  albums[index] = updatedAlbum
  return updatedAlbum
}

export function remove(id: number): boolean {
  const initialLength = albums.length
  albums = albums.filter(album => album.id !== id)
  return albums.length < initialLength
}
