import { sampleAlbums } from '../data/sampleAlbums.js'
import type { Album, AlbumRequest } from '../types.js'

const cloneAlbum = (album: Album): Album => ({
  ...album,
  artist: { ...album.artist },
})

export class AlbumsStore {
  private albums: Album[]

  constructor(seed: Album[] = sampleAlbums) {
    this.albums = seed.map(cloneAlbum)
  }

  list(): Album[] {
    return this.albums.map(cloneAlbum)
  }

  getById(id: number): Album | undefined {
    const album = this.albums.find((item) => item.id === id)
    return album ? cloneAlbum(album) : undefined
  }

  create(request: AlbumRequest): Album {
    const nextId = this.albums.length === 0 ? 1 : Math.max(...this.albums.map((album) => album.id)) + 1
    const album: Album = {
      id: nextId,
      ...request,
      artist: { ...request.artist },
    }

    this.albums.push(album)
    return cloneAlbum(album)
  }

  update(id: number, request: AlbumRequest): Album | undefined {
    const index = this.albums.findIndex((album) => album.id === id)
    if (index === -1) {
      return undefined
    }

    const updatedAlbum: Album = {
      id,
      ...request,
      artist: { ...request.artist },
    }

    this.albums[index] = updatedAlbum
    return cloneAlbum(updatedAlbum)
  }

  delete(id: number): boolean {
    const index = this.albums.findIndex((album) => album.id === id)
    if (index === -1) {
      return false
    }

    this.albums.splice(index, 1)
    return true
  }

  reset(seed: Album[] = sampleAlbums): void {
    this.albums = seed.map(cloneAlbum)
  }
}