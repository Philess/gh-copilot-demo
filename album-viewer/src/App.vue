<template>
  <div class="app">
    <header class="header">
      <div>
        <h1>🎵 Album Collection</h1>
        <p>Discover amazing music albums</p>
      </div>
      <button class="cart-toggle" type="button" aria-label="Toggle cart" @click="toggleCart">
        <span class="cart-icon">🛒</span>
        <span v-if="itemCount > 0" class="cart-badge">{{ itemCount }}</span>
      </button>
    </header>

    <main class="main">
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
          :is-in-cart="isInCart(album.id)"
          @add-to-cart="addToCart"
        />
      </div>
    </main>

    <div v-if="isCartOpen" class="cart-backdrop" @click="closeCart"></div>
    <CartPanel
      :items="items"
      :item-count="itemCount"
      :open="isCartOpen"
      @close="closeCart"
      @remove="removeFromCart"
    />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import CartPanel from './components/CartPanel.vue'
import { useCart } from './composables/useCart'
import type { Album } from './types/album'

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)
const isCartOpen = ref<boolean>(false)
const { items, itemCount, addToCart, removeFromCart, isInCart } = useCart()

// Fetch albums from the API
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

const toggleCart = (): void => {
  isCartOpen.value = !isCartOpen.value
}

const closeCart = (): void => {
  isCartOpen.value = false
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
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 1rem;
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
  position: relative;
  border: none;
  background: rgba(255, 255, 255, 0.16);
  color: white;
  width: 58px;
  height: 58px;
  border-radius: 50%;
  cursor: pointer;
  backdrop-filter: blur(10px);
  box-shadow: 0 10px 24px rgba(15, 23, 42, 0.22);
}

.cart-toggle:hover {
  background: rgba(255, 255, 255, 0.24);
}

.cart-icon {
  font-size: 1.5rem;
}

.cart-badge {
  position: absolute;
  top: -4px;
  right: -4px;
  min-width: 24px;
  height: 24px;
  padding: 0 0.35rem;
  border-radius: 999px;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  background: #ff8c42;
  color: white;
  font-size: 0.8rem;
  font-weight: 700;
}

.main {
  max-width: 1200px;
  margin: 0 auto;
}

.cart-backdrop {
  position: fixed;
  inset: 0;
  background: rgba(15, 23, 42, 0.32);
  z-index: 10;
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

  .header {
    align-items: flex-start;
  }
  
  .header h1 {
    font-size: 2rem;
  }

  .header p {
    margin-bottom: 0;
  }
  
  .albums-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
}
</style>
