export interface Artist {
  name: string
  birthdate: string
  birthPlace: string
}

export interface Album {
  id: number
  title: string
  artist: Artist
  year: number
  price: number
  image_url: string
}

export type AlbumInput = Omit<Album, 'id'>
