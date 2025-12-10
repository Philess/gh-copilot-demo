import { ref, computed, watch } from 'vue';
import type { Album } from '../types/album';

export interface CartItem {
  album: Album;
  qty: number;
}

const STORAGE_KEY = 'album-viewer-cart';

const cartItems = ref<CartItem[]>([]);

function load() {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (raw) cartItems.value = JSON.parse(raw);
  } catch {
    cartItems.value = [];
  }
}

function save() {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(cartItems.value));
}

watch(cartItems, save, { deep: true });

load();

export function useCart() {
  const count = computed(() => cartItems.value.reduce((sum, i) => sum + i.qty, 0));
  const subtotal = computed(() => cartItems.value.reduce((sum, i) => sum + i.album.price * i.qty, 0));

  function add(album: Album) {
    const existing = cartItems.value.find(i => i.album.id === album.id);
    if (existing) existing.qty += 1;
    else cartItems.value.push({ album, qty: 1 });
  }

  function remove(albumId: number) {
    const idx = cartItems.value.findIndex(i => i.album.id === albumId);
    if (idx !== -1) {
      const item = cartItems.value[idx];
      item.qty -= 1;
      if (item.qty <= 0) cartItems.value.splice(idx, 1);
    }
  }

  function clear() {
    cartItems.value = [];
  }

  return { cartItems, count, subtotal, add, remove, clear };
}
