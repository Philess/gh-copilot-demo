import { describe, it, expect, beforeEach, vi } from 'vitest'
import { useCart } from '../useCart'
import type { Album } from '../../types/album'

const sample: Album = {
  id: 1,
  title: 'Test Album',
  artist: 'Test Artist',
  price: 9.99,
  image_url: 'https://example.com/1.jpg'
}

describe('useCart', () => {
  beforeEach(() => {
    // provide a minimal localStorage for node test environment
    if (typeof globalThis.localStorage === 'undefined') {
      let store: Record<string, string> = {}
      globalThis.localStorage = {
        getItem: (k: string) => (k in store ? store[k] : null),
        setItem: (k: string, v: string) => (store[k] = String(v)),
        removeItem: (k: string) => delete store[k],
        clear: () => (store = {}),
      } as unknown as Storage
    }
    localStorage.clear()
  })

  it('adds and removes items and updates count', () => {
    const { items, count, addToCart, removeFromCart, clearCart } = useCart()
    expect(count.value).toBe(0)

    addToCart(sample)
    expect(count.value).toBe(1)
    expect(items.value[0].id).toBe(sample.id)

    // adding the same item again should not duplicate
    addToCart(sample)
    expect(count.value).toBe(1)

    removeFromCart(sample.id)
    expect(count.value).toBe(0)

    addToCart(sample)
    clearCart()
    expect(count.value).toBe(0)
  })

  it('persists to localStorage', () => {
    const { addToCart } = useCart()
    addToCart(sample)

    // create a new instance to simulate reload
    const { items, count } = useCart()
    expect(count.value).toBe(1)
    expect(items.value[0].id).toBe(sample.id)
  })
})