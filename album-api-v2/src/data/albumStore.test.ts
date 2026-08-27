import { describe, it, expect, beforeEach } from 'vitest'
import * as albumStore from './albumStore'
import { AlbumInput } from '../models/album'

// snapshot/restore the module-level in-memory array around each test
let snapshot: ReturnType<typeof albumStore.getAll>

beforeEach(() => {
  snapshot = albumStore.getAll()
})

function resetToSnapshot() {
  for (const album of albumStore.getAll()) {
    if (!snapshot.find(a => a.id === album.id)) {
      albumStore.remove(album.id)
    }
  }
}

describe('albumStore', () => {
  it('getAll returns the 6 seeded albums', () => {
    const albums = albumStore.getAll()
    expect(albums).toHaveLength(6)
    expect(albums[0]).toEqual({
      id: 1,
      title: 'You, Me and an App Id',
      artist: { name: 'Daprize', birthdate: '1988-04-12', birthPlace: 'Seattle' },
      year: 2021,
      price: 10.99,
      image_url: 'https://aka.ms/albums-daprlogo'
    })
  })

  it('getById returns the matching album', () => {
    const album = albumStore.getById(3)
    expect(album?.title).toBe('Scale It Up')
  })

  it('getById returns undefined for a missing id', () => {
    expect(albumStore.getById(9999)).toBeUndefined()
  })

  it('getByYear filters albums by year', () => {
    const albums = albumStore.getByYear(2020)
    expect(albums).toHaveLength(1)
    expect(albums[0].title).toBe('Seven Revision Army')
  })

  it('create assigns the next sequential id', () => {
    const input: AlbumInput = {
      title: 'New Album',
      artist: { name: 'New Artist', birthdate: '2000-01-01', birthPlace: 'Nowhere' },
      year: 2024,
      price: 9.99,
      image_url: 'https://example.com/image.png'
    }

    const created = albumStore.create(input)
    expect(created.id).toBe(7)
    expect(albumStore.getById(7)).toEqual(created)

    resetToSnapshot()
  })

  it('update replaces album fields but keeps the requested id', () => {
    const input: AlbumInput = {
      title: 'Updated Title',
      artist: { name: 'Updated Artist', birthdate: '1999-05-05', birthPlace: 'Elsewhere' },
      year: 2022,
      price: 19.99,
      image_url: 'https://example.com/updated.png'
    }

    const updated = albumStore.update(1, input)
    expect(updated).toEqual({ ...input, id: 1 })
    expect(albumStore.getById(1)?.title).toBe('Updated Title')

    // restore original album 1
    albumStore.update(1, {
      title: 'You, Me and an App Id',
      artist: { name: 'Daprize', birthdate: '1988-04-12', birthPlace: 'Seattle' },
      year: 2021,
      price: 10.99,
      image_url: 'https://aka.ms/albums-daprlogo'
    })
  })

  it('update returns undefined for a missing id', () => {
    const input: AlbumInput = {
      title: 'Nope',
      artist: { name: 'Nobody', birthdate: '2000-01-01', birthPlace: 'Nowhere' },
      year: 2024,
      price: 1,
      image_url: 'https://example.com/nope.png'
    }

    expect(albumStore.update(9999, input)).toBeUndefined()
  })

  it('remove deletes an existing album and returns true', () => {
    const created = albumStore.create({
      title: 'Temp',
      artist: { name: 'Temp Artist', birthdate: '2000-01-01', birthPlace: 'Nowhere' },
      year: 2024,
      price: 1,
      image_url: 'https://example.com/temp.png'
    })

    expect(albumStore.remove(created.id)).toBe(true)
    expect(albumStore.getById(created.id)).toBeUndefined()
  })

  it('remove returns false for a missing id', () => {
    expect(albumStore.remove(9999)).toBe(false)
  })
})
