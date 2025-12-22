<template>
  <div class="cart-backdrop" @click.self="$emit('close')">
    <aside class="cart-drawer" role="dialog" aria-label="Cart" aria-modal="true">
      <header class="drawer-header">
        <h2>Cart</h2>
        <button class="close-btn" @click="$emit('close')" aria-label="Close cart">✕</button>
      </header>

      <div v-if="items.length === 0" class="empty">
        <p>Your cart is empty.</p>
      </div>

      <ul class="cart-list">
        <li v-for="item in items" :key="item.id" class="cart-item">
          <img :src="item.image_url" :alt="item.title" class="thumb" />
          <div class="meta">
            <div class="title">{{ item.title }}</div>
            <div class="artist">{{ item.artist }}</div>
          </div>
          <div class="actions">
            <button class="remove-btn" @click="remove(item.id)">Remove</button>
          </div>
        </li>
      </ul>

      <footer class="drawer-footer" v-if="items.length > 0">
        <div class="summary">Items: {{ count }}</div>
        <div class="placeholder-checkout">Checkout (placeholder)</div>
      </footer>
    </aside>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { useCart } from '../composables/useCart'

const { items, count, removeFromCart } = useCart()

function remove(id: number) {
  removeFromCart(id)
}
</script>

<style scoped>
.cart-backdrop {
  position: fixed;
  inset: 0;
  background: rgba(0,0,0,0.5);
  display: flex;
  justify-content: flex-end;
  z-index: 50;
}

.cart-drawer {
  width: 360px;
  max-width: 100%;
  height: 100%;
  background: white;
  padding: 1rem;
  box-shadow: -10px 0 30px rgba(0,0,0,0.2);
  display: flex;
  flex-direction: column;
}

.drawer-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  border-bottom: 1px solid #eee;
  padding-bottom: 0.5rem;
}

.cart-list {
  list-style: none;
  margin: 0;
  padding: 0.5rem 0;
  overflow: auto;
  flex: 1 1 auto;
}

.cart-item {
  display: flex;
  gap: 0.75rem;
  align-items: center;
  padding: 0.5rem 0;
  border-bottom: 1px solid #f4f4f4;
}

.thumb {
  width: 56px;
  height: 56px;
  object-fit: cover;
  border-radius: 6px;
}

.meta {
  flex: 1 1 auto;
}

.title {
  font-weight: 700;
}

.artist {
  font-size: 0.9rem;
  color: #666;
}

.remove-btn {
  background: transparent;
  color: #ff5252;
  border: 1px solid #ffb4b4;
  padding: 0.4rem 0.6rem;
  border-radius: 6px;
  font-weight: 700;
}

.drawer-footer {
  border-top: 1px solid #eee;
  padding-top: 0.75rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.placeholder-checkout {
  background: #667eea;
  color: white;
  padding: 0.6rem 0.9rem;
  border-radius: 8px;
}
</style>