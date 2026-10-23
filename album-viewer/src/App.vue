<template>
  <div class="app">
    <header class="header">
      <div class="header-content">
        <h1>🎵 Album Collection</h1>
        <p>Discover amazing music albums</p>
      </div>
      <button
        class="cart-toggle"
        type="button"
        :aria-label="`Cart, ${cartCount} ${cartCount === 1 ? 'album' : 'albums'}`"
        aria-controls="cart-details"
        :aria-expanded="cartOpen"
        @click="cartOpen = !cartOpen"
      >
        <svg aria-hidden="true" viewBox="0 0 24 24">
          <path d="M3 3h2l2.4 12.1a2 2 0 0 0 2 1.6h8.8a2 2 0 0 0 2-1.6L22 8H6" />
          <circle cx="10" cy="21" r="1" />
          <circle cx="18" cy="21" r="1" />
        </svg>
        <span class="cart-count" aria-hidden="true">{{ cartCount }}</span>
      </button>
    </header>

    <main class="main">
      <section
        v-show="cartOpen"
        id="cart-details"
        class="cart-panel"
        aria-labelledby="cart-title"
      >
        <h2 id="cart-title">Your Cart</h2>
        <p v-if="cart.length === 0" class="empty-cart">Your cart is empty.</p>
        <ul v-else class="cart-list">
          <li v-for="album in cart" :key="album.id" class="cart-item">
            <img :src="album.image_url" :alt="''" class="cart-image" />
            <div class="cart-item-info">
              <strong>{{ album.title }}</strong>
              <span>{{ album.artist }}</span>
            </div>
            <span class="cart-price">${{ album.price.toFixed(2) }}</span>
            <button
              class="remove-button"
              type="button"
              :aria-label="`Remove ${album.title} from cart`"
              @click="removeAlbum(album.id)"
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
          :in-cart="hasAlbum(album.id)"
          @add-to-cart="addAlbum"
        />
      </div>
    </main>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import { useCart } from './composables/useCart'
import type { Album } from './types/album'

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)
const cartOpen = ref<boolean>(false)
const { cart, count: cartCount, hasAlbum, addAlbum, removeAlbum } = useCart()

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
  position: relative;
  text-align: center;
  margin-bottom: 3rem;
  color: white;
}

.header-content {
  padding: 0 4rem;
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

.cart-toggle {
  position: absolute;
  top: 0.5rem;
  right: 0;
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.65rem 0.9rem;
  border: 2px solid white;
  border-radius: 999px;
  background: rgba(255, 255, 255, 0.18);
  color: white;
  cursor: pointer;
  font: inherit;
  font-weight: 600;
}

.cart-toggle:hover,
.cart-toggle:focus-visible {
  background: white;
  color: #667eea;
}

.cart-toggle svg {
  width: 1.5rem;
  height: 1.5rem;
  fill: none;
  stroke: currentColor;
  stroke-linecap: round;
  stroke-linejoin: round;
  stroke-width: 2;
}

.cart-count {
  min-width: 1.5rem;
  text-align: center;
}

.main {
  max-width: 1200px;
  margin: 0 auto;
}

.cart-panel {
  margin: 0 1rem 2rem;
  padding: 1.5rem;
  border-radius: 15px;
  background: rgba(255, 255, 255, 0.96);
  color: #333;
  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
}

.cart-panel h2 {
  margin: 0 0 1rem;
}

.empty-cart {
  margin: 0;
  color: #666;
}

.cart-list {
  display: grid;
  gap: 1rem;
  margin: 0;
  padding: 0;
  list-style: none;
}

.cart-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding-bottom: 1rem;
  border-bottom: 1px solid #e5e5e5;
}

.cart-item:last-child {
  padding-bottom: 0;
  border-bottom: 0;
}

.cart-image {
  width: 3.5rem;
  height: 3.5rem;
  border-radius: 6px;
  object-fit: cover;
}

.cart-item-info {
  display: grid;
  flex: 1;
  gap: 0.25rem;
}

.cart-item-info span {
  color: #666;
}

.cart-price {
  font-weight: 600;
  white-space: nowrap;
}

.remove-button {
  padding: 0.5rem 0.75rem;
  border: 1px solid #667eea;
  border-radius: 6px;
  background: transparent;
  color: #5267d5;
  cursor: pointer;
  font: inherit;
}

.remove-button:hover,
.remove-button:focus-visible {
  background: #667eea;
  color: white;
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
  
.header-content {
  padding: 3.5rem 0 0;
  }

.cart-toggle {
  top: 0;
  right: 0;
}

.header h1 {
  font-size: 2rem;
}
  
  .albums-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }

  .cart-item {
    flex-wrap: wrap;
  }

  .cart-item-info {
    min-width: calc(100% - 4.5rem);
  }
}
</style>
