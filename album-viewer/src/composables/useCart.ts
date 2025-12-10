import { ref, computed, watch } from 'vue'
import type { Album } from '../types/album'

export interface CartItem {
  album: Album
  quantity: number
}

export interface CartState {
  items: Record<number, CartItem>
}

const CART_STORAGE_KEY = 'album-viewer-cart'

// Reactive state
const items = ref<Record<number, CartItem>>({})

// Load from localStorage on initialization
function loadFromStorage() {
  try {
    const stored = localStorage.getItem(CART_STORAGE_KEY)
    if (stored) {
      const parsed = JSON.parse(stored) as CartState
      items.value = parsed.items || {}
    }
  } catch (error) {
    console.error('Failed to load cart from storage:', error)
    items.value = {}
  }
}

// Save to localStorage
function saveToStorage() {
  try {
    const state: CartState = { items: items.value }
    localStorage.setItem(CART_STORAGE_KEY, JSON.stringify(state))
  } catch (error) {
    console.error('Failed to save cart to storage:', error)
  }
}

// Watch for changes and persist
watch(items, () => {
  saveToStorage()
}, { deep: true })

// Initialize cart from storage
loadFromStorage()

export function useCart() {
  // Computed properties
  const count = computed(() => {
    return Object.values(items.value).reduce((sum, item) => sum + item.quantity, 0)
  })

  const total = computed(() => {
    return Object.values(items.value).reduce(
      (sum, item) => sum + item.album.price * item.quantity,
      0
    )
  })

  const itemsList = computed(() => {
    return Object.values(items.value)
  })

  const isEmpty = computed(() => {
    return Object.keys(items.value).length === 0
  })

  // Actions
  function add(album: Album) {
    const existingItem = items.value[album.id]
    if (existingItem) {
      existingItem.quantity++
    } else {
      items.value[album.id] = {
        album,
        quantity: 1
      }
    }
  }

  function remove(albumId: number) {
    delete items.value[albumId]
  }

  function updateQuantity(albumId: number, quantity: number) {
    if (quantity <= 0) {
      remove(albumId)
    } else if (items.value[albumId]) {
      items.value[albumId].quantity = quantity
    }
  }

  function clear() {
    items.value = {}
  }

  function hasItem(albumId: number): boolean {
    return !!items.value[albumId]
  }

  return {
    items: computed(() => items.value),
    itemsList,
    count,
    total,
    isEmpty,
    add,
    remove,
    updateQuantity,
    clear,
    hasItem
  }
}
