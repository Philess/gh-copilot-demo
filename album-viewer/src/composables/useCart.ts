import { computed, ref } from 'vue'
import type { Album } from '../types/album'

export const useCart = () => {
  const items = ref<Album[]>([])

  const count = computed(() => items.value.length)

  const has = (albumId: number): boolean =>
    items.value.some((item) => item.id === albumId)

  const add = (album: Album): void => {
    if (!has(album.id)) {
      items.value.push(album)
    }
  }

  const remove = (albumId: number): void => {
    items.value = items.value.filter((item) => item.id !== albumId)
  }

  return { items, count, has, add, remove }
}