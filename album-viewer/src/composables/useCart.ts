import { computed, ref } from 'vue'
import type { Album } from '../types/album'

export const useCart = () => {
  const cart = ref<Album[]>([])

  const addAlbum = (album: Album): void => {
    if (!cart.value.some((item) => item.id === album.id)) {
      cart.value = [...cart.value, album]
    }
  }

  const removeAlbum = (albumId: Album['id']): void => {
    cart.value = cart.value.filter((album) => album.id !== albumId)
  }

  const hasAlbum = (albumId: Album['id']): boolean =>
    cart.value.some((album) => album.id === albumId)

  return {
    cart,
    count: computed(() => cart.value.length),
    hasAlbum,
    addAlbum,
    removeAlbum,
  }
}
