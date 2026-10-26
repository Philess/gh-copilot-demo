import { describe, expect, it } from 'vitest'
import { useCart } from './useCart'
import type { Album } from '../types/album'

const album: Album = {
  id: 1,
  title: 'Test Album',
  artist: 'Test Artist',
  price: 9.99,
  image_url: 'https://example.com/album.jpg',
}

const secondAlbum: Album = {
  ...album,
  id: 2,
  title: 'Another Album',
}

describe('useCart', () => {
  it('adds albums and exposes the current count', () => {
    const cart = useCart()

    cart.addAlbum(album)
    cart.addAlbum(secondAlbum)

    expect(cart.cart.value).toEqual([album, secondAlbum])
    expect(cart.count.value).toBe(2)
    expect(cart.hasAlbum(album.id)).toBe(true)
    expect(cart.hasAlbum(secondAlbum.id)).toBe(true)
  })

  it('does not add the same album more than once', () => {
    const cart = useCart()

    cart.addAlbum(album)
    cart.addAlbum({ ...album })

    expect(cart.cart.value).toEqual([album])
    expect(cart.count.value).toBe(1)
  })

  it('removes an album and updates the count', () => {
    const cart = useCart()
    cart.addAlbum(album)
    cart.addAlbum(secondAlbum)

    cart.removeAlbum(album.id)

    expect(cart.cart.value).toEqual([secondAlbum])
    expect(cart.count.value).toBe(1)
    expect(cart.hasAlbum(album.id)).toBe(false)

    cart.removeAlbum(secondAlbum.id)

    expect(cart.cart.value).toEqual([])
    expect(cart.count.value).toBe(0)
    expect(cart.hasAlbum(secondAlbum.id)).toBe(false)
  })
})
