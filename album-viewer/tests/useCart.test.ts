import { describe, it, expect, vi, beforeEach } from 'vitest'
import { useCart } from '../src/composables/useCart'

const mockStorage: Record<string, string> = {}

beforeEach(() => {
  // mock localStorage
  vi.stubGlobal('localStorage', {
    getItem: (k: string) => mockStorage[k] ?? null,
    setItem: (k: string, v: string) => { mockStorage[k] = v },
    removeItem: (k: string) => { delete mockStorage[k] },
    clear: () => { for (const k of Object.keys(mockStorage)) delete mockStorage[k] },
    key: (i: number) => Object.keys(mockStorage)[i] ?? null,
    length: Object.keys(mockStorage).length,
  } as any)
  for (const k of Object.keys(mockStorage)) delete mockStorage[k]
})

describe('useCart', () => {
  it('adds and counts items', () => {
    const { add, count } = useCart()
    add({ id: 1, title: 'A', artist: 'B', price: 10, image_url: '' })
    add({ id: 1, title: 'A', artist: 'B', price: 10, image_url: '' })
    add({ id: 2, title: 'X', artist: 'Y', price: 5, image_url: '' })
    expect(count.value).toBe(3)
  })

  it('removes items and clears when qty hits 0', () => {
    const { add, remove, count } = useCart()
    add({ id: 1, title: 'A', artist: 'B', price: 10, image_url: '' })
    add({ id: 1, title: 'A', artist: 'B', price: 10, image_url: '' })
    remove(1)
    expect(count.value).toBe(1)
    remove(1)
    expect(count.value).toBe(0)
  })

  it('persists to localStorage', () => {
    const { add } = useCart()
    add({ id: 3, title: 'C', artist: 'D', price: 2, image_url: '' })
    // ensure storage wrote something
    const keys = Object.keys(mockStorage)
    expect(keys.length).toBeGreaterThan(0)
  })
})
