import { describe, expect, it } from 'vitest'

import type { Album } from '../types/album'
import { createCartStore } from './useCart'

const albumOne: Album = {
  id: 1,
  title: 'You, Me and an App Id',
  artist: 'Daprize',
  price: 10.99,
  image_url: 'https://aka.ms/albums-daprlogo',
}

const albumTwo: Album = {
  id: 2,
  title: 'Seven Revision Army',
  artist: 'The Blue-Green Stripes',
  price: 13.99,
  image_url: 'https://aka.ms/albums-containerappslogo',
}

describe('createCartStore', () => {
  it('adds albums and updates the item count', () => {
    const cart = createCartStore()

    cart.addToCart(albumOne)
    cart.addToCart(albumTwo)

    expect(cart.items.value).toEqual([albumOne, albumTwo])
    expect(cart.itemCount.value).toBe(2)
  })

  it('does not duplicate an album already in the cart', () => {
    const cart = createCartStore()

    cart.addToCart(albumOne)
    cart.addToCart(albumOne)

    expect(cart.items.value).toEqual([albumOne])
    expect(cart.itemCount.value).toBe(1)
  })

  it('removes albums by id and updates membership checks', () => {
    const cart = createCartStore()

    cart.addToCart(albumOne)
    cart.addToCart(albumTwo)
    cart.removeFromCart(albumOne.id)

    expect(cart.items.value).toEqual([albumTwo])
    expect(cart.itemCount.value).toBe(1)
    expect(cart.isInCart(albumOne.id)).toBe(false)
    expect(cart.isInCart(albumTwo.id)).toBe(true)
  })
})