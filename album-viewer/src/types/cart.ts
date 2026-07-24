import type { Album } from './album'

export interface CartItem extends Album {
  quantity: number
}

export interface Cart {
  items: CartItem[]
}
