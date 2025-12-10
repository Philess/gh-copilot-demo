import { ref, computed, watch } from 'vue'
import type { Album } from '../types/album'
import type { CartItem } from '../types/cart'

const STORAGE_KEY = 'album-cart'

// Global cart state
const cartItems = ref<CartItem[]>([])
const isCartOpen = ref(false)

// Load cart from localStorage on initialization
const loadCartFromStorage = (): void => {
  try {
    const stored = localStorage.getItem(STORAGE_KEY)
    if (stored) {
      const parsed = JSON.parse(stored)
      // Convert date strings back to Date objects
      cartItems.value = parsed.map((item: any) => ({
        ...item,
        addedAt: new Date(item.addedAt)
      }))
    }
  } catch (error) {
    console.error('Error loading cart from storage:', error)
    cartItems.value = []
  }
}

// Save cart to localStorage
const saveCartToStorage = (): void => {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(cartItems.value))
  } catch (error) {
    console.error('Error saving cart to storage:', error)
  }
}

// Initialize cart from storage
loadCartFromStorage()

// Watch for cart changes and save to storage
watch(cartItems, saveCartToStorage, { deep: true })

export const useCart = () => {
  // Computed properties
  const cartCount = computed(() => cartItems.value.length)
  
  const cartTotal = computed(() => {
    return cartItems.value.reduce((total, item) => total + item.album.price, 0)
  })

  // Check if album is in cart
  const isInCart = (albumId: number): boolean => {
    return cartItems.value.some(item => item.album.id === albumId)
  }

  // Add album to cart
  const addToCart = (album: Album): boolean => {
    if (isInCart(album.id)) {
      console.warn('Album already in cart')
      return false
    }

    const cartItem: CartItem = {
      album,
      addedAt: new Date()
    }

    cartItems.value.push(cartItem)
    return true
  }

  // Remove album from cart
  const removeFromCart = (albumId: number): boolean => {
    const index = cartItems.value.findIndex(item => item.album.id === albumId)
    if (index === -1) {
      console.warn('Album not found in cart')
      return false
    }

    cartItems.value.splice(index, 1)
    return true
  }

  // Clear entire cart
  const clearCart = (): void => {
    cartItems.value = []
  }

  // Toggle cart sidebar
  const toggleCart = (): void => {
    isCartOpen.value = !isCartOpen.value
  }

  // Open cart sidebar
  const openCart = (): void => {
    isCartOpen.value = true
  }

  // Close cart sidebar
  const closeCart = (): void => {
    isCartOpen.value = false
  }

  return {
    // State
    cartItems: computed(() => cartItems.value),
    isCartOpen: computed(() => isCartOpen.value),
    
    // Computed
    cartCount,
    cartTotal,
    
    // Methods
    addToCart,
    removeFromCart,
    isInCart,
    clearCart,
    toggleCart,
    openCart,
    closeCart
  }
}
