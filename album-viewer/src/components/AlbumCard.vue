<template>
  <div class="album-card">
    <div class="album-image">
      <img 
        :src="album.image_url" 
        :alt="album.title"
        @error="handleImageError"
        loading="lazy"
      />
      <div class="play-overlay">
        <div class="play-button">▶</div>
      </div>
    </div>
    
    <div class="album-info">
      <h3 class="album-title">{{ album.title }}</h3>
      <p class="album-artist">{{ album.artist }}</p>
      <div class="album-price">
        <span class="price">${{ album.price.toFixed(2) }}</span>
      </div>
    </div>
    
    <div class="album-actions">
      <button 
        class="btn btn-primary"
        :class="{ 'in-cart': isInCart }"
        @click="handleAddToCart"
      >
        {{ isInCart ? t.albumCard.inCart : t.albumCard.addToCart }}
      </button>
      <button class="btn btn-secondary">{{ t.albumCard.preview }}</button>
    </div>
  </div>
</template>

<script setup lang="ts">
import type { Album } from '../types/album'
import { useI18n } from '../composables/useI18n'
import { useCart } from '../composables/useCart'
import { computed } from 'vue'

interface Props {
  album: Album
}

const props = defineProps<Props>()

const { t } = useI18n()
const { addToCart, isInCart } = useCart()

const isAlbumInCart = computed(() => isInCart(props.album.id))

const handleAddToCart = () => {
  addToCart(props.album, 1)
}

const handleImageError = (event: Event): void => {
  const target = event.target as HTMLImageElement
  target.src = 'https://via.placeholder.com/300x300/667eea/white?text=Album+Cover'
}
</script>

<style scoped>
.album-card {
  background: rgba(255, 255, 255, 0.95);
  border-radius: 15px;
  overflow: hidden;
  box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
  transition: all 0.3s ease;
  backdrop-filter: blur(10px);
}

.album-card:hover {
  transform: translateY(-10px);
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.3);
}

.album-image {
  position: relative;
  overflow: hidden;
}

.album-image img {
  width: 100%;
  height: 250px;
  object-fit: cover;
  transition: transform 0.3s ease;
}

.album-card:hover .album-image img {
  transform: scale(1.1);
}

.play-overlay {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  opacity: 0;
  transition: opacity 0.3s ease;
}

.album-card:hover .play-overlay {
  opacity: 1;
}

.play-button {
  width: 60px;
  height: 60px;
  background: rgba(255, 255, 255, 0.9);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 1.5rem;
  color: #667eea;
  cursor: pointer;
  transition: all 0.3s ease;
}

.play-button:hover {
  background: white;
  transform: scale(1.1);
}

.album-info {
  padding: 1.5rem;
}

.album-title {
  font-size: 1.3rem;
  font-weight: bold;
  color: #333;
  margin: 0 0 0.5rem 0;
  line-height: 1.3;
}

.album-artist {
  color: #666;
  font-size: 1rem;
  margin: 0 0 1rem 0;
}

.album-price {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.price {
  font-size: 1.5rem;
  font-weight: bold;
  color: #667eea;
}

.album-actions {
  padding: 0 1.5rem 1.5rem;
  display: flex;
  gap: 0.75rem;
}

.btn {
  flex: 1;
  padding: 0.75rem;
  border: none;
  border-radius: 8px;
  font-size: 0.9rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.btn-primary {
  background: #667eea;
  color: white;
}

.btn-primary:hover {
  background: #5a6fd8;
  transform: translateY(-2px);
}

.btn-primary.in-cart {
  background: #4caf50;
  opacity: 0.8;
}

.btn-primary.in-cart:hover {
  background: #45a049;
  opacity: 1;
}

.btn-secondary {
  background: transparent;
  color: #667eea;
  border: 2px solid #667eea;
}

.btn-secondary:hover {
  background: #667eea;
  color: white;
  transform: translateY(-2px);
}

@media (max-width: 768px) {
  .album-info {
    padding: 1rem;
  }
  
  .album-actions {
    padding: 0 1rem 1rem;
    flex-direction: column;
  }
  
  .btn {
    width: 100%;
  }
}
</style>
