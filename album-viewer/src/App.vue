<template>
  <div class="app">
    <header class="header">
      <div class="header-content">
        <h1>🎵 Album Collection</h1>
        <button class="cart-icon-btn" @click="toggleCart">
          🛒
          <span v-if="cart.items.length > 0" class="cart-badge">{{ cartItemCount }}</span>
        </button>
      </div>
      <p>Discover amazing music albums</p>
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
          @add-to-cart="addToCart"
        />
      </div>
    </main>

    <CartView 
      v-if="showCart"
      :cart="cart"
      @close="closeCart"
      @remove="removeFromCart"
      @update-quantity="updateCartItemQuantity"
    />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, computed } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import CartView from './components/CartView.vue'
import type { Album } from './types/album'
import type { Cart, CartItem } from './types/cart'

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)
const showCart = ref<boolean>(false)
const cart = ref<Cart>({
  items: []
})

const cartItemCount = computed(() => {
  return cart.value.items.reduce((sum, item) => sum + item.quantity, 0)
})

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

const addToCart = (album: Album): void => {
  const existingItem = cart.value.items.find(item => item.id === album.id)
  
  if (existingItem) {
    existingItem.quantity += 1
  } else {
    const cartItem: CartItem = {
      ...album,
      quantity: 1
    }
    cart.value.items.push(cartItem)
  }
}

const removeFromCart = (albumId: number): void => {
  cart.value.items = cart.value.items.filter(item => item.id !== albumId)
}

const updateCartItemQuantity = (albumId: number, quantity: number): void => {
  const item = cart.value.items.find(i => i.id === albumId)
  if (item) {
    item.quantity = quantity
  }
}

const toggleCart = (): void => {
  showCart.value = !showCart.value
}

const closeCart = (): void => {
  showCart.value = false
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

.header-content {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 2rem;
  margin-bottom: 1rem;
}

.header h1 {
  font-size: 3rem;
  margin: 0;
  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
}

.cart-icon-btn {
  position: relative;
  background: rgba(255, 255, 255, 0.2);
  border: 2px solid white;
  color: white;
  font-size: 1.5rem;
  width: 60px;
  height: 60px;
  border-radius: 50%;
  cursor: pointer;
  transition: all 0.3s ease;
  display: flex;
  align-items: center;
  justify-content: center;
}

.cart-icon-btn:hover {
  background: rgba(255, 255, 255, 0.3);
  transform: scale(1.1);
}

.cart-badge {
  position: absolute;
  top: -5px;
  right: -5px;
  background: #ff4757;
  color: white;
  font-size: 0.8rem;
  font-weight: bold;
  width: 24px;
  height: 24px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.header p {
  font-size: 1.2rem;
  opacity: 0.9;
  margin: 0;
}

.main {
  max-width: 1200px;
  margin: 0 auto;
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
    flex-direction: column;
    gap: 1rem;
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
