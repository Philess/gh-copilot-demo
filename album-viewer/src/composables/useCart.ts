import { ref, computed, watch } from 'vue'
import type { Album } from '../types/album'
import type { CartItem } from '../types/cart'

const STORAGE_KEY = 'albumCartItems'
const MAX_CART_SIZE = 50

// Global state
const cartItems = ref<CartItem[]>([])
const isCartOpen = ref(false)

// Load cart from localStorage on initialization
const loadCartFromStorage = (): void => {
  try {
    const stored = localStorage.getItem(STORAGE_KEY)
    if (stored) {
      const parsed = JSON.parse(stored)
      if (Array.isArray(parsed)) {
        cartItems.value = parsed
      }
    }
  } catch (error) {
    console.error('Failed to load cart from localStorage:', error)
    cartItems.value = []
  }
}

// Save cart to localStorage
const saveCartToStorage = (): void => {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(cartItems.value))
  } catch (error) {
    console.error('Failed to save cart to localStorage:', error)
  }
}

// Initialize cart on first import
loadCartFromStorage()

// Watch for changes and save to localStorage
watch(cartItems, saveCartToStorage, { deep: true })

export function useCart() {
  const addToCart = (album: Album): boolean => {
    // Check if album already exists in cart
    const existingItem = cartItems.value.find(item => item.album.id === album.id)
    
    if (existingItem) {
      return false // Album already in cart
    }
    
    // Check max cart size
    if (cartItems.value.length >= MAX_CART_SIZE) {
      console.warn(`Cart is full. Maximum ${MAX_CART_SIZE} items allowed.`)
      return false
    }
    
    // Add new item to cart
    const newItem: CartItem = {
      album,
      quantity: 1,
      addedAt: new Date().toISOString()
    }
    
    cartItems.value.push(newItem)
    return true
  }

  const removeFromCart = (albumId: number): void => {
    const index = cartItems.value.findIndex(item => item.album.id === albumId)
    if (index !== -1) {
      cartItems.value.splice(index, 1)
    }
  }

  const clearCart = (): void => {
    cartItems.value = []
  }

  const isInCart = (albumId: number): boolean => {
    return cartItems.value.some(item => item.album.id === albumId)
  }

  const getCartItemCount = computed((): number => {
    return cartItems.value.length
  })

  const getCartTotal = computed((): number => {
    return cartItems.value.reduce((total, item) => {
      return total + (item.album.price * item.quantity)
    }, 0)
  })

  const toggleCart = (): void => {
    isCartOpen.value = !isCartOpen.value
  }

  const openCart = (): void => {
    isCartOpen.value = true
  }

  const closeCart = (): void => {
    isCartOpen.value = false
  }

  return {
    cartItems: computed(() => cartItems.value),
    isCartOpen: computed(() => isCartOpen.value),
    addToCart,
    removeFromCart,
    clearCart,
    isInCart,
    getCartItemCount,
    getCartTotal,
    toggleCart,
    openCart,
    closeCart
  }
}
