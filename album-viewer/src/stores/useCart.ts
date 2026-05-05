import { ref, computed } from 'vue'
import type { Album, CartItem } from '../types/album'

const CART_STORAGE_KEY = 'album-viewer-cart'

const cartItems = ref<CartItem[]>(loadCartFromStorage())

function loadCartFromStorage(): CartItem[] {
  try {
    const stored = localStorage.getItem(CART_STORAGE_KEY)
    return stored ? JSON.parse(stored) : []
  } catch {
    return []
  }
}

function saveCartToStorage(): void {
  localStorage.setItem(CART_STORAGE_KEY, JSON.stringify(cartItems.value))
}

export function useCart() {
  const cartCount = computed(() => {
    return cartItems.value.reduce((sum, item) => sum + item.quantity, 0)
  })

  const totalPrice = computed(() => {
    return cartItems.value.reduce((sum, item) => sum + item.album.price * item.quantity, 0)
  })

  function addToCart(album: Album): void {
    const existingItem = cartItems.value.find(item => item.album.id === album.id)
    
    if (existingItem) {
      existingItem.quantity++
    } else {
      cartItems.value.push({ album, quantity: 1 })
    }
    
    saveCartToStorage()
  }

  function removeFromCart(albumId: number): void {
    cartItems.value = cartItems.value.filter(item => item.album.id !== albumId)
    saveCartToStorage()
  }

  function decrementQuantity(albumId: number): void {
    const item = cartItems.value.find(i => i.album.id === albumId)
    if (item) {
      if (item.quantity > 1) {
        item.quantity--
      } else {
        removeFromCart(albumId)
      }
      saveCartToStorage()
    }
  }

  function incrementQuantity(albumId: number): void {
    const item = cartItems.value.find(i => i.album.id === albumId)
    if (item) {
      item.quantity++
      saveCartToStorage()
    }
  }

  function clearCart(): void {
    cartItems.value = []
    saveCartToStorage()
  }

  return {
    cartItems,
    cartCount,
    totalPrice,
    addToCart,
    removeFromCart,
    decrementQuantity,
    incrementQuantity,
    clearCart
  }
}
