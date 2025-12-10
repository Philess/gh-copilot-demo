<template>
  <div class="app">
    <header class="header">
      <div class="header-top">
        <div class="header-content">
          <h1>🎵 {{ t.header.title }}</h1>
          <p>{{ t.header.subtitle }}</p>
        </div>
        <div class="header-actions">
          <LanguageSelector />
          <CartIcon />
        </div>
      </div>
    </header>

    <main class="main">
      <div v-if="loading" class="loading">
        <div class="spinner"></div>
        <p>{{ t.loading.message }}</p>
      </div>

      <div v-else-if="error" class="error">
        <p>{{ t.error.message }}</p>
        <button @click="fetchAlbums" class="retry-btn">{{ t.error.retry }}</button>
      </div>

      <div v-else class="albums-grid">
        <AlbumCard 
          v-for="album in albums" 
          :key="album.id" 
          :album="album" 
        />
      </div>
    </main>
    
    <CartSidebar />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import LanguageSelector from './components/LanguageSelector.vue'
import CartIcon from './components/CartIcon.vue'
import CartSidebar from './components/CartSidebar.vue'
import type { Album } from './types/album'
import { useI18n } from './i18n'

const { t } = useI18n()

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)

const fetchAlbums = async (): Promise<void> => {
  try {
    loading.value = true
    error.value = null
    const response = await axios.get<Album[]>('/albums')
    albums.value = response.data
  } catch (err) {
    error.value = t.value.error.message
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
.header-top {
  display: flex;
  justify-content: space-between;
  align-items: center;
  max-width: 1200px;
  margin: 0 auto;
  gap: 2rem;
}

.header-content {
  flex: 1;
  text-align: center;
}

.header-actions {
  display: flex;
  align-items: center;
  gap: 1.5rem;
}
.header-content {
  flex: 1;
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
@media (max-width: 768px) {
  .app {
    padding: 1rem;
  }
  
  .header-top {
    flex-direction: column;
    align-items: stretch;
    gap: 1rem;
  }
  
  .header-content {
    text-align: center;
  }
  
  .header-actions {
    justify-content: center;
    gap: 1rem;
  }
  
  .header h1 {
    font-size: 2rem;
  }
  
  .albums-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
} }
  
  .albums-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
}media (max-width: 768px) {
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
