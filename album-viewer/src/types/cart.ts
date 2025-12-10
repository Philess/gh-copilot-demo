import type { Album } from './album'

export interface CartItem {
  album: Album
  addedAt: Date
}

export interface CartState {
  items: CartItem[]
  isOpen: boolean
}
