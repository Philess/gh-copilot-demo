import type { Album } from './album'

export interface CartItem {
  album: Album
  quantity: number
  addedAt: string
}

export interface CartState {
  items: CartItem[]
  isOpen: boolean
}
