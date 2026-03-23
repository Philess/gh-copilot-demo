import { computed, ref } from 'vue'

import type { Album } from '../types/album'

export const createCartStore = () => {
  const items = ref<Album[]>([])

  const itemCount = computed(() => items.value.length)

  const addToCart = (album: Album): void => {
    if (items.value.some((item) => item.id === album.id)) {
      return
    }

    items.value = [...items.value, album]
  }

  const removeFromCart = (albumId: number): void => {
    items.value = items.value.filter((item) => item.id !== albumId)
  }

  const isInCart = (albumId: number): boolean => {
    return items.value.some((item) => item.id === albumId)
  }

  return {
    items,
    itemCount,
    addToCart,
    removeFromCart,
    isInCart,
  }
}

export const useCart = () => createCartStore()