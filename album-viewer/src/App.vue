<template>
  <div class="app">
    <header class="header">
      <div style="display:flex;align-items:center;justify-content:space-between;gap:16px;">
        <div>
          <h1>🎵 {{ $t('app.title') }}</h1>
          <p>{{ $t('app.subtitle') }}</p>
        </div>
        <div style="display:flex;align-items:center;gap:1rem;">
          <div class="lang-select">
            <label for="lang">{{ $t('language.label') }}</label>
            <select id="lang" v-model="locale" @change="onChangeLocale">
              <option value="en">{{ $t('language.en') }}</option>
              <option value="fr">{{ $t('language.fr') }}</option>
              <option value="de">{{ $t('language.de') }}</option>
            </select>
          </div>
          <CartIcon :count="cart.count.value" @toggle="toggleCart" />
        </div>
      </div>
    </header>

    <main class="main">
      <div v-if="loading" class="loading">
        <div class="spinner"></div>
        <p>{{ $t('loading.albums') }}</p>
      </div>

      <div v-else-if="error" class="error">
        <p>{{ $t('errors.loadFailed') }}</p>
        <button @click="fetchAlbums" class="retry-btn">{{ $t('errors.retry') }}</button>
      </div>

      <div v-else class="albums-grid">
        <AlbumCard 
          v-for="album in albums" 
          :key="album.id" 
          :album="album" 
        />
      </div>
    </main>

    <CartDrawer 
      :is-open="isCartOpen"
      :items-list="cart.itemsList.value"
      :total="cart.total.value"
      :is-empty="cart.isEmpty.value"
      @close="toggleCart"
      @remove="cart.remove"
      @update-quantity="cart.updateQuantity"
      @clear="cart.clear"
    />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, watch } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import CartIcon from './components/CartIcon.vue'
import CartDrawer from './components/CartDrawer.vue'
import type { Album } from './types/album'
import { setLocale, i18n } from './i18n'
import { useCart } from './composables/useCart'

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)
const locale = ref<string>(localStorage.getItem('locale') || 'en')
const isCartOpen = ref<boolean>(false)

const cart = useCart()

// Update page title when locale changes
watch(locale, () => {
  document.title = i18n.global.t('app.pageTitle')
}, { immediate: true })

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

function onChangeLocale() {
  setLocale(locale.value as 'en' | 'fr' | 'de')
}

function toggleCart() {
  isCartOpen.value = !isCartOpen.value
}
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
