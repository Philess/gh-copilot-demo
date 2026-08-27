import { describe, expect, it } from 'vitest'
import type { Album } from '../types/album'
import { useCart } from './useCart'

const album: Album = {
  id: 1,
  title: 'Test Album',
  artist: { name: 'Test Artist', birthdate: '', birthPlace: '' },
  year: 2024,
  price: 12.5,
  image_url: 'cover.jpg'
}

describe('useCart', () => {
  it('adds albums and prevents duplicate entries', () => {
    const cart = useCart()

    cart.add(album)
    cart.add(album)

    expect(cart.items.value).toEqual([album])
    expect(cart.count.value).toBe(1)
    expect(cart.has(album.id)).toBe(true)
  })

  it('removes an album and updates the count', () => {
    const cart = useCart()
    cart.add(album)

    cart.remove(album.id)

    expect(cart.items.value).toEqual([])
    expect(cart.count.value).toBe(0)
    expect(cart.has(album.id)).toBe(false)
  })
})