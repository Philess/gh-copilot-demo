<template>
  <div class="app">
    <header class="header">
      <div class="header-content">
        <div>
          <h1>{{ t.header.title }}</h1>
          <p>{{ t.header.subtitle }}</p>
        </div>
        <div class="lang-selector">
          <select :value="currentLocale" @change="setLocale(($event.target as HTMLSelectElement).value as Locale)" aria-label="Select language">
            <option value="en">{{ t.languages.en }}</option>
            <option value="fr">{{ t.languages.fr }}</option>
            <option value="de">{{ t.languages.de }}</option>
          </select>
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
        <button @click="fetchAlbums" class="retry-btn">{{ t.retry }}</button>
      </div>

      <div v-else class="albums-grid">
        <AlbumCard 
          v-for="album in albums" 
          :key="album.id" 
          :album="album" 
        />
      </div>
    </main>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue'
import axios from 'axios'
import AlbumCard from './components/AlbumCard.vue'
import type { Album } from './types/album'
import { useI18n } from './i18n'
import type { Locale } from './i18n'

const { t, currentLocale, setLocale } = useI18n()

const albums = ref<Album[]>([])
const loading = ref<boolean>(true)
const error = ref<string | null>(null)

// Function to fetch albums from the API
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

.header-content {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 2rem;
  flex-wrap: wrap;
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

.lang-selector select {
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid rgba(255, 255, 255, 0.6);
  border-radius: 20px;
  padding: 0.4rem 1rem;
  font-size: 0.95rem;
  cursor: pointer;
  outline: none;
  appearance: none;
  -webkit-appearance: none;
  transition: background 0.2s ease;
}

.lang-selector select:hover,
.lang-selector select:focus {
  background: rgba(255, 255, 255, 0.35);
}

.lang-selector select option {
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
