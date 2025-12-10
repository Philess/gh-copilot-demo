<template>
  <div v-if="isOpen" class="cart-drawer-overlay" @click="close">
    <div class="cart-drawer" @click.stop>
      <div class="cart-header">
        <h2>{{ t.cart.title }}</h2>
        <button class="close-btn" @click="close">✕</button>
      </div>

      <div v-if="items.length === 0" class="cart-empty">
        <span class="empty-icon">🛒</span>
        <p>{{ t.cart.empty }}</p>
      </div>

      <div v-else class="cart-content">
        <div class="cart-items">
          <div v-for="item in items" :key="item.album.id" class="cart-item">
            <img :src="item.album.image_url" :alt="item.album.title" class="item-image" />
            <div class="item-info">
              <h3 class="item-title">{{ item.album.title }}</h3>
              <p class="item-artist">{{ item.album.artist }}</p>
              <div class="item-price">
                <span>${{ (item.album.price * item.quantity).toFixed(2) }}</span>
              </div>
            </div>
            <div class="item-actions">
              <div class="quantity-control">
                <button @click="decreaseQuantity(item.album.id)" class="qty-btn">−</button>
                <span class="qty-display">{{ item.quantity }}</span>
                <button @click="increaseQuantity(item.album.id)" class="qty-btn">+</button>
              </div>
              <button @click="removeItem(item.album.id)" class="remove-btn">
                {{ t.cart.removeFromCart }}
              </button>
            </div>
          </div>
        </div>

        <div class="cart-summary">
          <div class="summary-row">
            <span>{{ t.cart.subtotal }}:</span>
            <span>${{ total.toFixed(2) }}</span>
          </div>
          <div class="summary-row total">
            <span>{{ t.cart.total }}:</span>
            <span>${{ total.toFixed(2) }}</span>
          </div>
        </div>

        <div class="cart-actions">
          <button @click="clearAllItems" class="btn btn-secondary">
            {{ t.cart.clearCart }}
          </button>
          <button class="btn btn-primary">{{ t.cart.checkout }}</button>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useCart } from '../composables/useCart'
import { useI18n } from '../composables/useI18n'

defineProps<{
  isOpen: boolean
}>()

const emit = defineEmits<{
  close: []
}>()

const { items, total, removeFromCart, updateQuantity, clearCart } = useCart()
const { t } = useI18n()

const close = () => {
  emit('close')
}

const removeItem = (albumId: number) => {
  removeFromCart(albumId)
}

const increaseQuantity = (albumId: number) => {
  const item = items.value.find(i => i.album.id === albumId)
  if (item) {
    updateQuantity(albumId, item.quantity + 1)
  }
}

const decreaseQuantity = (albumId: number) => {
  const item = items.value.find(i => i.album.id === albumId)
  if (item) {
    updateQuantity(albumId, item.quantity - 1)
  }
}

const clearAllItems = () => {
  if (confirm(`${t.value.cart.clearCart}?`)) {
    clearCart()
  }
}
</script>

<style scoped>
.cart-drawer-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  z-index: 999;
  animation: fadeIn 0.3s ease;
}

@keyframes fadeIn {
  from {
    opacity: 0;
  }
  to {
    opacity: 1;
  }
}

.cart-drawer {
  position: fixed;
  top: 0;
  right: 0;
  width: 400px;
  height: 100vh;
  background: white;
  box-shadow: -2px 0 10px rgba(0, 0, 0, 0.1);
  display: flex;
  flex-direction: column;
  z-index: 1000;
  animation: slideIn 0.3s ease;
}

@keyframes slideIn {
  from {
    transform: translateX(400px);
  }
  to {
    transform: translateX(0);
  }
}

.cart-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem;
  border-bottom: 1px solid #eee;
}

.cart-header h2 {
  margin: 0;
  font-size: 1.5rem;
  color: #333;
}

.close-btn {
  background: none;
  border: none;
  font-size: 1.5rem;
  cursor: pointer;
  color: #666;
  transition: color 0.3s ease;
  padding: 0;
  width: 32px;
  height: 32px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.close-btn:hover {
  color: #333;
}

.cart-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  flex: 1;
  color: #999;
  padding: 2rem;
  text-align: center;
}

.empty-icon {
  font-size: 3rem;
  margin-bottom: 1rem;
  opacity: 0.5;
}

.cart-content {
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: hidden;
}

.cart-items {
  flex: 1;
  overflow-y: auto;
  padding: 1rem;
  border-bottom: 1px solid #eee;
}

.cart-item {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  background: #f9f9f9;
  border-radius: 8px;
  margin-bottom: 1rem;
  align-items: flex-start;
}

.item-image {
  width: 80px;
  height: 80px;
  object-fit: cover;
  border-radius: 4px;
  flex-shrink: 0;
}

.item-info {
  flex: 1;
  min-width: 0;
}

.item-title {
  margin: 0 0 0.25rem 0;
  font-size: 0.95rem;
  color: #333;
  font-weight: 600;
  word-break: break-word;
}

.item-artist {
  margin: 0 0 0.5rem 0;
  font-size: 0.85rem;
  color: #666;
}

.item-price {
  font-size: 1rem;
  font-weight: bold;
  color: #667eea;
}

.item-actions {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  align-items: flex-start;
}

.quantity-control {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  border: 1px solid #ddd;
  border-radius: 4px;
  background: white;
}

.qty-btn {
  background: none;
  border: none;
  cursor: pointer;
  padding: 0.25rem 0.5rem;
  color: #667eea;
  font-weight: bold;
  transition: background 0.2s ease;
}

.qty-btn:hover {
  background: #f0f0f0;
}

.qty-display {
  padding: 0 0.5rem;
  min-width: 25px;
  text-align: center;
  font-weight: 600;
}

.remove-btn {
  background: #fff3cd;
  color: #856404;
  border: none;
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  font-size: 0.8rem;
  cursor: pointer;
  transition: all 0.2s ease;
}

.remove-btn:hover {
  background: #ffeaa7;
}

.cart-summary {
  padding: 1rem;
  background: #f9f9f9;
  border-top: 1px solid #eee;
}

.summary-row {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.5rem;
  color: #666;
  font-size: 0.95rem;
}

.summary-row.total {
  font-weight: bold;
  font-size: 1.1rem;
  color: #333;
  border-top: 1px solid #ddd;
  padding-top: 0.5rem;
  margin-top: 0.5rem;
  color: #667eea;
}

.cart-actions {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
  padding: 1rem;
}

.btn {
  padding: 0.75rem 1rem;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  font-size: 0.95rem;
}

.btn-primary {
  background: #667eea;
  color: white;
}

.btn-primary:hover {
  background: #5a6fd8;
  transform: translateY(-2px);
}

.btn-secondary {
  background: transparent;
  color: #667eea;
  border: 2px solid #667eea;
}

.btn-secondary:hover {
  background: #667eea;
  color: white;
}

@media (max-width: 768px) {
  .cart-drawer {
    width: 100%;
  }

  @keyframes slideIn {
    from {
      transform: translateX(100%);
    }
    to {
      transform: translateX(0);
    }
  }
}
</style>
