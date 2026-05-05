<template>
  <div class="app">
    <header class="header">
      <div class="header-content">
        <div class="header-text">
          <h1>{{ t.header.title }}</h1>
          <p>{{ t.header.subtitle }}</p>
        </div>
        <div class="header-controls">
          <div class="language-selector">
            <select v-model="currentLocale" @change="setLocale(currentLocale)" class="lang-select">
              <option v-for="(label, code) in localeLabels" :key="code" :value="code">
                {{ label }}
              </option>
            </select>
          </div>
          <button class="cart-toggle-btn" @click="showCart = !showCart">
            {{ t.cart.button }} ({{ cartItemCount }})
          </button>
        </div>
      </div>
    </header>

    <main class="main">
      <div v-if="loading" class="loading">
        <div class="spinner"></div>
        <p>{{ t.loading }}</p>
      </div>

      <div v-else-if="error" class="error">
        <p>{{ error }}</p>
        <button @click="fetchAlbums" class="retry-btn">{{ t.retryButton }}</button>
      </div>

      <div v-else :class="['content-layout', { 'cart-open': showCart }]">
        <section class="albums-grid">
          <AlbumCard
            v-for="album in albums"
            :key="album.id"
            :album="album"
            :quantity="getQuantity(album.id)"
            @add-to-cart="addToCart(album)"
            @remove-from-cart="removeFromCart(album.id)"
          />
        </section>

        <aside v-if="showCart" class="cart-panel">
          <h2>{{ t.cart.title }}</h2>

          <p v-if="cartItemCount === 0" class="cart-empty">{{ t.cart.empty }}</p>

          <div v-else class="cart-content">
            <ul class="cart-list">
              <li v-for="item in cart" :key="item.id" class="cart-item">
                <div>
                  <p class="cart-item-title">{{ item.title }}</p>
                  <p class="cart-item-meta">{{ item.quantity }} x ${{ item.price.toFixed(2) }}</p>
                </div>
                <p class="cart-item-total">${{ (item.price * item.quantity).toFixed(2) }}</p>
              </li>
            </ul>

            <div class="cart-summary">
              <p>{{ t.cart.items }}: <strong>{{ cartItemCount }}</strong></p>
              <p>{{ t.cart.total }}: <strong>${{ cartTotal.toFixed(2) }}</strong></p>
            </div>

            <button class="clear-cart-btn" @click="clearCart">{{ t.cart.clear }}</button>
          </div>
        </aside>
      </div>
    </main>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import type { Album } from './types/album'
import { useI18n } from './composables/useI18n'

const { t, currentLocale, setLocale, localeLabels } = useI18n()

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)
const cart = ref<Array<Album & { quantity: number }>>([])
const showCart = ref<boolean>(false)

const cartItemCount = computed(() =>
  cart.value.reduce((total, item) => total + item.quantity, 0),
)

const cartTotal = computed(() =>
  cart.value.reduce((total, item) => total + item.price * item.quantity, 0),
)

const getQuantity = (albumId: number): number => {
  const item = cart.value.find((cartItem) => cartItem.id === albumId)
  return item?.quantity ?? 0
}

const addToCart = (album: Album): void => {
  const existingItem = cart.value.find((cartItem) => cartItem.id === album.id)
  if (existingItem) {
    existingItem.quantity += 1
    return
  }

  cart.value.push({ ...album, quantity: 1 })
}

const removeFromCart = (albumId: number): void => {
  const index = cart.value.findIndex((cartItem) => cartItem.id === albumId)
  if (index === -1) {
    return
  }

  const item = cart.value[index]
  if (!item) {
    return
  }

  if (item.quantity > 1) {
    item.quantity -= 1
    return
  }

  cart.value.splice(index, 1)
}

const clearCart = (): void => {
  cart.value = []
}

const fetchAlbums = async (): Promise<void> => {
  try {
    loading.value = true
    error.value = null
    const response = await axios.get<Album[]>('/albums')
    albums.value = response.data
  } catch (err) {
    error.value = t.value.error
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
  margin-bottom: 3rem;
  color: white;
}

.header-content {
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
}

.header-controls {
  position: absolute;
  right: 0;
  display: flex;
  gap: 0.75rem;
  align-items: center;
}

.header-text {
  text-align: center;
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

.language-selector {
  position: static;
}

.cart-toggle-btn {
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid rgba(255, 255, 255, 0.6);
  border-radius: 8px;
  padding: 0.5rem 0.9rem;
  font-size: 0.95rem;
  font-weight: 600;
  cursor: pointer;
  backdrop-filter: blur(4px);
}

.cart-toggle-btn:hover {
  background: rgba(255, 255, 255, 0.3);
  border-color: white;
}

.lang-select {
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid rgba(255, 255, 255, 0.6);
  border-radius: 8px;
  padding: 0.5rem 0.75rem;
  font-size: 0.95rem;
  cursor: pointer;
  backdrop-filter: blur(4px);
  transition: all 0.3s ease;
  appearance: none;
  padding-right: 2rem;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 12 12'%3E%3Cpath fill='white' d='M6 8L1 3h10z'/%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 0.5rem center;
}

.lang-select:hover,
.lang-select:focus {
  background-color: rgba(255, 255, 255, 0.3);
  border-color: white;
  outline: none;
}

.lang-select option {
  background: #667eea;
  color: white;
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

.content-layout {
  display: grid;
  grid-template-columns: minmax(0, 1fr);
  gap: 1.5rem;
  align-items: start;
}

.content-layout.cart-open {
  grid-template-columns: minmax(0, 3fr) minmax(260px, 1fr);
}

.albums-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 2rem;
}

.cart-panel {
  position: sticky;
  top: 1rem;
  background: rgba(255, 255, 255, 0.95);
  border-radius: 15px;
  padding: 1rem;
  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
}

.cart-panel h2 {
  margin: 0 0 1rem;
  color: #334;
}

.cart-empty {
  color: #666;
  margin: 0;
}

.cart-list {
  list-style: none;
  margin: 0;
  padding: 0;
  display: grid;
  gap: 0.75rem;
}

.cart-item {
  display: flex;
  justify-content: space-between;
  gap: 1rem;
  border-bottom: 1px solid #e2e2f0;
  padding-bottom: 0.75rem;
}

.cart-item-title {
  margin: 0;
  font-weight: 600;
  color: #222;
}

.cart-item-meta {
  margin: 0.2rem 0 0;
  color: #666;
  font-size: 0.9rem;
}

.cart-item-total {
  margin: 0;
  font-weight: 700;
  color: #667eea;
}

.cart-summary {
  margin-top: 1rem;
  border-top: 1px solid #e2e2f0;
  padding-top: 0.75rem;
  color: #222;
}

.cart-summary p {
  margin: 0.25rem 0;
}

.clear-cart-btn {
  margin-top: 1rem;
  width: 100%;
  border: none;
  border-radius: 8px;
  padding: 0.75rem;
  background: #334;
  color: #fff;
  font-weight: 600;
  cursor: pointer;
}

.clear-cart-btn:hover {
  background: #1f2538;
}

@media (max-width: 768px) {
  .app {
    padding: 1rem;
  }
  
  .header h1 {
    font-size: 2rem;
  }

  .header-content {
    flex-direction: column;
    gap: 1rem;
  }

  .header-controls {
    position: static;
  }

  .language-selector {
    position: static;
  }
  
  .content-layout,
  .content-layout.cart-open {
    grid-template-columns: 1fr;
  }

  .cart-panel {
    position: static;
  }

  .albums-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
}
</style>
