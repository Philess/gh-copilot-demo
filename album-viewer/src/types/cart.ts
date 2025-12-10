import type { Album } from './album'

export interface CartItem {
  album: Album
  quantity: number
  addedAt: Date
}

export interface Cart {
  items: CartItem[]
  total: number
  itemCount: number
}
