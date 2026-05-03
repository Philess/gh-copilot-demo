import { ref, computed } from 'vue'
import type { Album } from '../types/album'

export interface CartItem extends Album {
  quantity: number
}

const cartItems = ref<CartItem[]>([])
const isCartOpen = ref(false)

export function useCart() {
  const addToCart = (album: Album) => {
    const existingItem = cartItems.value.find(item => item.id === album.id)
    
    if (existingItem) {
      existingItem.quantity++
    } else {
      cartItems.value.push({ ...album, quantity: 1 })
    }
  }

  const removeFromCart = (albumId: number) => {
    const index = cartItems.value.findIndex(item => item.id === albumId)
    if (index !== -1) {
      cartItems.value.splice(index, 1)
    }
  }

  const updateQuantity = (albumId: number, quantity: number) => {
    const item = cartItems.value.find(item => item.id === albumId)
    if (item) {
      if (quantity <= 0) {
        removeFromCart(albumId)
      } else {
        item.quantity = quantity
      }
    }
  }

  const clearCart = () => {
    cartItems.value = []
  }

  const toggleCart = () => {
    isCartOpen.value = !isCartOpen.value
  }

  const openCart = () => {
    isCartOpen.value = true
  }

  const closeCart = () => {
    isCartOpen.value = false
  }

  const cartTotal = computed(() => {
    return cartItems.value.reduce((total, item) => {
      return total + (item.price * item.quantity)
    }, 0)
  })

  const cartCount = computed(() => {
    return cartItems.value.reduce((count, item) => count + item.quantity, 0)
  })

  return {
    cartItems,
    isCartOpen,
    addToCart,
    removeFromCart,
    updateQuantity,
    clearCart,
    toggleCart,
    openCart,
    closeCart,
    cartTotal,
    cartCount
  }
}
