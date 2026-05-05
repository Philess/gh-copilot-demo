<template>
  <div v-if="isOpen" class="cart-overlay" @click.self="$emit('close')">
    <div class="cart-drawer">
      <div class="cart-header">
        <h2>{{ t.cart.title }}</h2>
        <button class="close-btn" @click="$emit('close')" aria-label="Close cart">✕</button>
      </div>

      <div v-if="cartItems.length === 0" class="cart-empty">
        <p>{{ t.cart.empty }}</p>
      </div>

      <ul v-else class="cart-list">
        <li v-for="item in cartItems" :key="item.album.id" class="cart-item">
          <img :src="item.album.image_url" :alt="item.album.title" class="cart-item-img" />
          <div class="cart-item-info">
            <p class="cart-item-title">{{ item.album.title }}</p>
            <p class="cart-item-artist">{{ item.album.artist.name }}</p>
            <p class="cart-item-price">${{ item.album.price.toFixed(2) }}</p>
          </div>
          <button class="remove-btn" @click="removeFromCart(item.album.id)" :aria-label="t.cart.remove">
            {{ t.cart.remove }}
          </button>
        </li>
      </ul>

      <div v-if="cartItems.length > 0" class="cart-footer">
        <span class="cart-item-count">{{ t.cart.itemCount.replace('{count}', String(cartCount)) }}</span>
        <span class="cart-total">{{ t.cart.total }}: ${{ cartTotal.toFixed(2) }}</span>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useCart } from '../composables/useCart'
import { useI18n } from '../i18n'

defineProps<{ isOpen: boolean }>()
defineEmits<{ (e: 'close'): void }>()

const { cartItems, cartCount, cartTotal, removeFromCart } = useCart()
const { t } = useI18n()
</script>

<style scoped>
.cart-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.5);
  z-index: 100;
  display: flex;
  justify-content: flex-end;
}

.cart-drawer {
  background: white;
  width: 380px;
  max-width: 100vw;
  height: 100%;
  display: flex;
  flex-direction: column;
  box-shadow: -4px 0 20px rgba(0, 0, 0, 0.3);
  overflow: hidden;
}

.cart-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1.25rem 1.5rem;
  border-bottom: 1px solid #eee;
  background: #667eea;
  color: white;
}

.cart-header h2 {
  margin: 0;
  font-size: 1.3rem;
}

.close-btn {
  background: transparent;
  border: none;
  color: white;
  font-size: 1.2rem;
  cursor: pointer;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  transition: background 0.2s;
}

.close-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

.cart-empty {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  color: #999;
  font-size: 1rem;
}

.cart-list {
  list-style: none;
  margin: 0;
  padding: 0;
  flex: 1;
  overflow-y: auto;
}

.cart-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem 1.5rem;
  border-bottom: 1px solid #f0f0f0;
}

.cart-item-img {
  width: 60px;
  height: 60px;
  object-fit: cover;
  border-radius: 8px;
  flex-shrink: 0;
}

.cart-item-info {
  flex: 1;
  min-width: 0;
}

.cart-item-title {
  font-weight: 600;
  color: #333;
  margin: 0 0 0.2rem 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.cart-item-artist {
  color: #666;
  font-size: 0.85rem;
  margin: 0 0 0.2rem 0;
}

.cart-item-price {
  color: #667eea;
  font-weight: bold;
  margin: 0;
}

.remove-btn {
  background: transparent;
  color: #e55;
  border: 1px solid #e55;
  border-radius: 6px;
  padding: 0.3rem 0.6rem;
  font-size: 0.8rem;
  cursor: pointer;
  white-space: nowrap;
  transition: all 0.2s;
}

.remove-btn:hover {
  background: #e55;
  color: white;
}

.cart-footer {
  padding: 1.25rem 1.5rem;
  border-top: 1px solid #eee;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}

.cart-item-count {
  color: #888;
  font-size: 0.9rem;
}

.cart-total {
  font-size: 1.2rem;
  font-weight: bold;
  color: #333;
  text-align: right;
}
</style>
