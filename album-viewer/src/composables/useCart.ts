import { ref, computed } from 'vue'
import type { Album } from '../types/album'
import type { CartItem } from '../types/cart'

const cartItems = ref<CartItem[]>([])

export function useCart() {
  const cartCount = computed(() => cartItems.value.length)

  const cartTotal = computed(() =>
    cartItems.value.reduce((sum, item) => sum + item.album.price * item.quantity, 0)
  )

  function addToCart(album: Album): void {
    if (!cartItems.value.find(item => item.album.id === album.id)) {
      cartItems.value.push({ album, quantity: 1 })
    }
  }

  function removeFromCart(albumId: number): void {
    cartItems.value = cartItems.value.filter(item => item.album.id !== albumId)
  }

  function isInCart(albumId: number): boolean {
    return cartItems.value.some(item => item.album.id === albumId)
  }

  return { cartItems, cartCount, cartTotal, addToCart, removeFromCart, isInCart }
}
