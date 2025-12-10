import { ref, computed } from 'vue'
import type { Album } from '../types/album'
import type { CartItem } from '../types/cart'

const STORAGE_KEY = 'album-cart'

const cartItems = ref<CartItem[]>([])

// Initialize from localStorage
function initializeCart() {
  const stored = localStorage.getItem(STORAGE_KEY)
  if (stored) {
    try {
      const parsed = JSON.parse(stored)
      cartItems.value = parsed.map((item: any) => ({
        ...item,
        addedAt: new Date(item.addedAt)
      }))
    } catch (error) {
      console.error('Error loading cart from localStorage:', error)
      cartItems.value = []
    }
  }
}

// Save cart to localStorage
function saveCart() {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(cartItems.value))
}

export function useCart() {
  const items = computed(() => cartItems.value)

  const itemCount = computed(() => {
    return cartItems.value.reduce((sum, item) => sum + item.quantity, 0)
  })

  const total = computed(() => {
    return cartItems.value.reduce((sum, item) => sum + item.album.price * item.quantity, 0)
  })

  const addToCart = (album: Album, quantity: number = 1): void => {
    const existingItem = cartItems.value.find(item => item.album.id === album.id)

    if (existingItem) {
      existingItem.quantity += quantity
    } else {
      cartItems.value.push({
        album,
        quantity,
        addedAt: new Date()
      })
    }

    saveCart()
  }

  const removeFromCart = (albumId: number): void => {
    const index = cartItems.value.findIndex(item => item.album.id === albumId)
    if (index !== -1) {
      cartItems.value.splice(index, 1)
      saveCart()
    }
  }

  const updateQuantity = (albumId: number, quantity: number): void => {
    const item = cartItems.value.find(item => item.album.id === albumId)
    if (item) {
      if (quantity <= 0) {
        removeFromCart(albumId)
      } else {
        item.quantity = quantity
        saveCart()
      }
    }
  }

  const clearCart = (): void => {
    cartItems.value = []
    saveCart()
  }

  const isInCart = (albumId: number): boolean => {
    return cartItems.value.some(item => item.album.id === albumId)
  }

  const getCartItem = (albumId: number): CartItem | undefined => {
    return cartItems.value.find(item => item.album.id === albumId)
  }

  // Initialize on first call
  if (cartItems.value.length === 0 && !localStorage.getItem(STORAGE_KEY)) {
    initializeCart()
  } else if (cartItems.value.length === 0) {
    initializeCart()
  }

  return {
    items,
    itemCount,
    total,
    addToCart,
    removeFromCart,
    updateQuantity,
    clearCart,
    isInCart,
    getCartItem
  }
}
