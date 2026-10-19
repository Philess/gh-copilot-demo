<template>
  <div class="app">
    <header class="header">
      <h1>🎵 Album Collection</h1>
      <p>Discover amazing music albums</p>
      <button
        ref="cartTrigger"
        type="button"
        class="cart-trigger"
        :aria-label="`Cart, ${cartCount} albums`"
        :aria-expanded="cartOpen"
        aria-controls="cart-panel"
        @click="toggleCart"
      >
        <svg aria-hidden="true" viewBox="0 0 24 24" width="24" height="24" fill="none" stroke="currentColor" stroke-width="2">
          <path d="M2 3h3l3 12h11l3-9H6" />
          <circle cx="9" cy="20" r="1" />
          <circle cx="18" cy="20" r="1" />
        </svg>
        Cart <span class="cart-count" aria-hidden="true">{{ cartCount }}</span>
      </button>
      <span class="sr-only" role="status" aria-live="polite" aria-atomic="true">
        {{ cartCount }} albums in cart
      </span>
    </header>

    <main class="main">
      <section
        v-if="cartOpen"
        id="cart-panel"
        ref="cartPanel"
        class="cart-panel"
        aria-labelledby="cart-title"
        tabindex="-1"
        @keydown.esc.stop.prevent="closeCart"
      >
        <div class="cart-heading">
          <h2 id="cart-title">Your cart</h2>
          <button ref="closeCartButton" type="button" class="cart-action" @click="closeCart">
            Close cart
          </button>
        </div>
        <p v-if="cartCount === 0">Your cart is empty</p>
        <ul v-else class="cart-items">
          <li v-for="album in cart" :key="album.id" class="cart-item">
            <div class="cart-item-info">
              <h3>{{ album.title }}</h3>
              <p>{{ album.artist }}</p>
              <span>${{ album.price.toFixed(2) }}</span>
            </div>
            <button
              type="button"
              class="cart-action"
              data-cart-remove
              :aria-label="`Remove ${album.title} by ${album.artist} from cart`"
              @click="removeFromCart(album.id)"
            >
              Remove
            </button>
          </li>
        </ul>
      </section>

      <div v-if="loading" class="loading">
        <div class="spinner"></div>
        <p>Loading albums...</p>
      </div>

      <div v-else-if="error" class="error">
        <p>{{ error }}</p>
        <button @click="fetchAlbums" class="retry-btn">Try Again</button>
      </div>

      <div v-else class="albums-grid">
        <AlbumCard 
          v-for="album in albums" 
          :key="album.id" 
          :album="album" 
          :in-cart="cartIds.has(album.id)"
          @add-to-cart="addToCart"
        />
      </div>
    </main>
  </div>
</template>

<script setup lang="ts">
import { computed, nextTick, ref, onMounted } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import type { Album } from './types/album'

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)
const cart = ref<Album[]>([])
const cartIds = computed(() => new Set(cart.value.map(album => album.id)))
const cartCount = computed(() => cart.value.length)
const cartOpen = ref(false)
const cartTrigger = ref<HTMLButtonElement | null>(null)
const cartPanel = ref<HTMLElement | null>(null)
const closeCartButton = ref<HTMLButtonElement | null>(null)

const addToCart = (album: Album): void => {
  if (!cartIds.value.has(album.id)) {
    cart.value.push(album)
  }
}

const closeCart = async (): Promise<void> => {
  cartOpen.value = false
  await nextTick()
  cartTrigger.value?.focus()
}

const toggleCart = async (): Promise<void> => {
  if (cartOpen.value) {
    await closeCart()
    return
  }
  cartOpen.value = true
  await nextTick()
  cartPanel.value?.focus()
}

const removeFromCart = async (id: number): Promise<void> => {
  const index = cart.value.findIndex(album => album.id === id)
  const buttons = cartPanel.value?.querySelectorAll<HTMLButtonElement>('[data-cart-remove]')
  const restoreFocus = buttons?.[index] === document.activeElement
  cart.value = cart.value.filter(album => album.id !== id)
  await nextTick()
  if (restoreFocus) {
    const remaining = cartPanel.value?.querySelectorAll<HTMLButtonElement>('[data-cart-remove]')
    const target = remaining?.[Math.min(index, cart.value.length - 1)] ?? closeCartButton.value
    target?.focus()
  }
}

const fetchAlbums = async (): Promise<void> => {
  try {
    loading.value = true
    error.value = null
    const response = await axios.get<Album[]>('/albums')
    albums.value = response.data
  } catch (err) {
    error.value = 'Failed to load albums. Please make sure the API is running.'
    console.error('Error fetching albums:', err)
  } finally {
    loading.value = false
  }
}

onMounted(() => {
  fetchAlbums()
})
</script>

<style scoped>
.app {
  min-height: 100vh;
  padding: 2rem;
}

.header {
  text-align: center;
  margin-bottom: 3rem;
  color: white;
}

.header h1 {
  font-size: 3rem;
  margin-bottom: 0.5rem;
  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
}

.header p {
  font-size: 1.2rem;
  opacity: 0.9;
}

.main {
  max-width: 1200px;
  margin: 0 auto;
}

.cart-trigger,
.cart-action {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  min-height: 44px;
  padding: 0.65rem 1rem;
  border: 2px solid #667eea;
  border-radius: 8px;
  background: white;
  color: #3949a0;
  font: inherit;
  cursor: pointer;
}

.cart-trigger {
  margin-top: 1rem;
}

.cart-count {
  min-width: 1.5rem;
  border-radius: 1rem;
  background: #3949a0;
  color: white;
}

.cart-trigger:focus-visible,
.cart-action:focus-visible,
.cart-panel:focus-visible {
  outline: 3px solid #222;
  outline-offset: 3px;
}

.cart-panel {
  padding: 1.5rem;
  margin-bottom: 2rem;
  border-radius: 15px;
  background: white;
  color: #333;
  overflow-wrap: anywhere;
}

.cart-heading,
.cart-item {
  display: flex;
  align-items: center;
  justify-content: space-between;
  flex-wrap: wrap;
  gap: 1rem;
}

.cart-heading h2,
.cart-item h3,
.cart-item p {
  margin: 0 0 0.5rem;
}

.cart-item-info {
  flex: 1 1 12rem;
  min-width: 0;
}

.cart-items {
  list-style: none;
  margin: 0;
  padding: 0;
}

.cart-item {
  padding: 1rem 0;
  border-top: 1px solid #ddd;
}

.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip-path: inset(50%);
  white-space: nowrap;
  border: 0;
}

.loading {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 4rem;
  color: white;
}

.spinner {
  width: 50px;
  height: 50px;
  border: 4px solid rgba(255, 255, 255, 0.3);
  border-top: 4px solid white;
  border-radius: 50%;
  animation: spin 1s linear infinite;
  margin-bottom: 1rem;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

.error {
  text-align: center;
  padding: 4rem;
  color: white;
}

.error p {
  font-size: 1.2rem;
  margin-bottom: 2rem;
}

.retry-btn {
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid white;
  padding: 0.75rem 2rem;
  border-radius: 25px;
  font-size: 1rem;
  cursor: pointer;
  transition: all 0.3s ease;
}

.retry-btn:hover {
  background: white;
  color: #667eea;
}

.albums-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 2rem;
  padding: 1rem;
}

@media (max-width: 768px) {
  .app {
    padding: 1rem;
  }
  
  .header h1 {
    font-size: 2rem;
  }
  
  .albums-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
}
</style>
