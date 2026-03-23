export interface Artist {
  name: string
  birthdate: string
  birthPlace: string
}

export interface AlbumRequest {
  title: string
  artist: Artist
  year: number
  price: number
  image_url: string
}

export interface Album extends AlbumRequest {
  id: number
}