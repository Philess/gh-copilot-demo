import { ref, computed, watch } from 'vue'
import type { Album } from '../types/album'

const STORAGE_KEY = 'album-viewer-cart'

export function useCart() {
  const items = ref<Album[]>([])

  // initialize from localStorage
  try {
    const raw = localStorage.getItem(STORAGE_KEY)
    if (raw) items.value = JSON.parse(raw) as Album[]
  } catch (e) {
    // ignore parse errors
    console.error('Failed to load cart from localStorage', e)
  }

  const count = computed(() => items.value.length)

  function save(itemsToSave: Album[]) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(itemsToSave))
    } catch (e) {
      console.error('Failed to save cart to localStorage', e)
    }
  }

  function addToCart(album: Album) {
    if (!items.value.find(a => a.id === album.id)) {
      items.value = [...items.value, album]
      save(items.value)
    }
  }

  function removeFromCart(albumId: number) {
    items.value = items.value.filter(a => a.id !== albumId)
    save(items.value)
  }

  function clearCart() {
    items.value = []
    save(items.value)
  }

  // persist (kept as backup if external mutations change items)
  watch(
    items,
    (val) => {
      save(val)
    },
    { deep: true }
  )

  return { items, count, addToCart, removeFromCart, clearCart }
}
