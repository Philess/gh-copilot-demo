<template>
  <div class="cart-backdrop" role="presentation" @click.self="emit('close')">
    <aside class="cart-panel" role="dialog" aria-modal="true" aria-labelledby="cart-title">
      <div class="cart-header">
        <h2 id="cart-title">Your Cart ({{ items.length }})</h2>
        <button class="close-button" aria-label="Close cart" @click="emit('close')">&times;</button>
      </div>

      <p v-if="items.length === 0" class="empty-cart">Your cart is empty.</p>

      <ul v-else class="cart-items">
        <li v-for="album in items" :key="album.id" class="cart-item">
          <img :src="album.image_url" :alt="album.title" @error="handleImageError" />
          <div class="cart-item-info">
            <h3>{{ album.title }}</h3>
            <p>{{ album.artist.name }}</p>
            <strong>${{ album.price.toFixed(2) }}</strong>
          </div>
          <button class="remove-button" :aria-label="`Remove ${album.title} from cart`" @click="emit('remove', album.id)">
            Remove
          </button>
        </li>
      </ul>
    </aside>
  </div>
</template>

<script setup lang="ts">
import { onMounted, onUnmounted } from 'vue'
import type { Album } from '../types/album'

defineProps<{
  items: Album[]
}>()

const emit = defineEmits<{
  close: []
  remove: [albumId: number]
}>()

const handleKeydown = (event: KeyboardEvent): void => {
  if (event.key === 'Escape') {
    emit('close')
  }
}

onMounted(() => window.addEventListener('keydown', handleKeydown))
onUnmounted(() => window.removeEventListener('keydown', handleKeydown))

const handleImageError = (event: Event): void => {
  const target = event.target as HTMLImageElement
  target.src = 'https://via.placeholder.com/80x80/667eea/white?text=Album'
}
</script>

<style scoped>
.cart-backdrop {
  position: fixed;
  inset: 0;
  z-index: 10;
  display: flex;
  justify-content: flex-end;
  background: rgba(0, 0, 0, 0.45);
}

.cart-panel {
  width: min(100%, 420px);
  height: 100%;
  overflow-y: auto;
  padding: 1.5rem;
  box-sizing: border-box;
  background: white;
  color: #333;
  box-shadow: -8px 0 24px rgba(0, 0, 0, 0.2);
}

.cart-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 1rem;
  border-bottom: 1px solid #e5e7eb;
  padding-bottom: 1rem;
}

.cart-header h2 {
  margin: 0;
}

.close-button,
.remove-button {
  border: 0;
  cursor: pointer;
}

.close-button {
  background: transparent;
  color: #555;
  font-size: 1.75rem;
  line-height: 1;
}

.empty-cart {
  color: #666;
  text-align: center;
  padding: 3rem 1rem;
}

.cart-items {
  display: grid;
  gap: 1rem;
  list-style: none;
  margin: 1.25rem 0 0;
  padding: 0;
}

.cart-item {
  display: grid;
  grid-template-columns: 64px 1fr auto;
  align-items: center;
  gap: 0.75rem;
  padding-bottom: 1rem;
  border-bottom: 1px solid #e5e7eb;
}

.cart-item img {
  width: 64px;
  height: 64px;
  border-radius: 6px;
  object-fit: cover;
}

.cart-item-info h3,
.cart-item-info p {
  margin: 0 0 0.25rem;
}

.cart-item-info h3 {
  font-size: 1rem;
}

.cart-item-info p {
  color: #666;
  font-size: 0.9rem;
}

.remove-button {
  background: transparent;
  color: #b42318;
  font-size: 0.85rem;
  padding: 0.5rem 0;
}

button:focus-visible {
  outline: 3px solid #f59e0b;
  outline-offset: 2px;
}

@media (max-width: 480px) {
  .cart-item {
    grid-template-columns: 56px 1fr;
  }

  .cart-item img {
    width: 56px;
    height: 56px;
  }

  .remove-button {
    grid-column: 2;
    justify-self: start;
  }
}
</style>