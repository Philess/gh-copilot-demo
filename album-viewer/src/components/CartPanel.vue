<template>
  <aside
    class="cart-panel"
    :class="{ 'cart-panel-open': open }"
    aria-label="Shopping cart"
  >
    <div class="cart-header">
      <div>
        <p class="cart-kicker">Your cart</p>
        <h2>{{ itemCount }} {{ itemCount === 1 ? 'album' : 'albums' }}</h2>
      </div>
      <button class="close-btn" type="button" aria-label="Close cart" @click="emit('close')">
        ×
      </button>
    </div>

    <div v-if="items.length === 0" class="cart-empty">
      <p>Your cart is empty.</p>
      <span>Add albums from the collection to see them here.</span>
    </div>

    <ul v-else class="cart-list">
      <li v-for="item in items" :key="item.id" class="cart-item">
        <img :src="item.image_url" :alt="item.title" class="cart-cover" />
        <div class="cart-item-info">
          <h3>{{ item.title }}</h3>
          <p>{{ item.artist }}</p>
          <span>${{ item.price.toFixed(2) }}</span>
        </div>
        <button class="remove-btn" type="button" @click="emit('remove', item.id)">
          Remove
        </button>
      </li>
    </ul>
  </aside>
</template>

<script setup lang="ts">
import type { Album } from '../types/album'

interface Props {
  items: Album[]
  itemCount: number
  open: boolean
}

defineProps<Props>()

const emit = defineEmits<{
  close: []
  remove: [albumId: number]
}>()
</script>

<style scoped>
.cart-panel {
  position: fixed;
  top: 0;
  right: 0;
  width: min(420px, 100%);
  height: 100vh;
  background: rgba(255, 255, 255, 0.98);
  color: #1c2340;
  box-shadow: -12px 0 32px rgba(15, 23, 42, 0.22);
  transform: translateX(100%);
  transition: transform 0.3s ease;
  z-index: 20;
  display: flex;
  flex-direction: column;
}

.cart-panel-open {
  transform: translateX(0);
}

.cart-header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  padding: 1.5rem;
  border-bottom: 1px solid rgba(102, 126, 234, 0.15);
}

.cart-kicker {
  margin: 0 0 0.3rem;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  font-size: 0.75rem;
  color: #667eea;
}

.cart-header h2 {
  margin: 0;
  font-size: 1.5rem;
}

.close-btn {
  border: none;
  background: transparent;
  color: #1c2340;
  font-size: 2rem;
  line-height: 1;
  cursor: pointer;
}

.cart-empty {
  padding: 2rem 1.5rem;
  display: grid;
  gap: 0.5rem;
}

.cart-empty p {
  margin: 0;
  font-size: 1.1rem;
  font-weight: 700;
}

.cart-empty span {
  color: #54607a;
}

.cart-list {
  list-style: none;
  padding: 1rem 1.5rem 1.5rem;
  margin: 0;
  display: grid;
  gap: 1rem;
  overflow-y: auto;
}

.cart-item {
  display: grid;
  grid-template-columns: 72px 1fr auto;
  gap: 0.9rem;
  align-items: center;
  padding: 0.85rem;
  border-radius: 14px;
  background: #f6f8ff;
}

.cart-cover {
  width: 72px;
  height: 72px;
  object-fit: cover;
  border-radius: 10px;
}

.cart-item-info h3,
.cart-item-info p,
.cart-item-info span {
  margin: 0;
}

.cart-item-info {
  display: grid;
  gap: 0.2rem;
}

.cart-item-info p {
  color: #54607a;
}

.cart-item-info span {
  color: #667eea;
  font-weight: 700;
}

.remove-btn {
  border: none;
  background: #eef2ff;
  color: #4252b8;
  border-radius: 999px;
  padding: 0.65rem 0.9rem;
  cursor: pointer;
  font-weight: 700;
}

.remove-btn:hover {
  background: #dce5ff;
}

@media (max-width: 768px) {
  .cart-panel {
    width: 100%;
  }

  .cart-item {
    grid-template-columns: 56px 1fr;
  }

  .remove-btn {
    grid-column: 1 / -1;
  }
}
</style>