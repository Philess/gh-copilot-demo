<template>
  <div class="cart-modal">
    <div class="cart-overlay" @click="closeCart"></div>
    <div class="cart-panel">
      <div class="cart-header">
        <h2>Shopping Cart</h2>
        <button class="close-btn" @click="closeCart">✕</button>
      </div>

      <div class="cart-content">
        <div v-if="cart.items.length === 0" class="empty-cart">
          <p>Your cart is empty</p>
          <p class="empty-subtitle">Add albums to get started</p>
        </div>

        <div v-else class="cart-items">
          <div v-for="item in cart.items" :key="item.id" class="cart-item">
            <img :src="item.image_url" :alt="item.title" class="item-image" />
            <div class="item-info">
              <h3>{{ item.title }}</h3>
              <p class="item-artist">{{ item.artist }}</p>
              <p class="item-price">${{ item.price.toFixed(2) }}</p>
            </div>
            <div class="item-quantity">
              <button @click="decrementQuantity(item.id)" class="qty-btn">−</button>
              <span class="qty-value">{{ item.quantity }}</span>
              <button @click="incrementQuantity(item.id)" class="qty-btn">+</button>
            </div>
            <button @click="removeFromCart(item.id)" class="remove-btn">🗑️</button>
          </div>
        </div>
      </div>

      <div v-if="cart.items.length > 0" class="cart-footer">
        <div class="cart-total">
          <span>Total:</span>
          <span class="total-price">${{ calculateTotal().toFixed(2) }}</span>
        </div>
        <button class="checkout-btn">Proceed to Checkout</button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import type { Cart, CartItem } from '../types/cart'

interface Props {
  cart: Cart
}

interface Emits {
  (e: 'close'): void
  (e: 'remove', albumId: number): void
  (e: 'update-quantity', albumId: number, quantity: number): void
}

const props = defineProps<Props>()
const emit = defineEmits<Emits>()

const closeCart = () => {
  emit('close')
}

const removeFromCart = (albumId: number) => {
  emit('remove', albumId)
}

const incrementQuantity = (albumId: number) => {
  const item = props.cart.items.find(i => i.id === albumId)
  if (item) {
    emit('update-quantity', albumId, item.quantity + 1)
  }
}

const decrementQuantity = (albumId: number) => {
  const item = props.cart.items.find(i => i.id === albumId)
  if (item && item.quantity > 1) {
    emit('update-quantity', albumId, item.quantity - 1)
  }
}

const calculateTotal = (): number => {
  return props.cart.items.reduce((sum, item) => sum + item.price * item.quantity, 0)
}
</script>

<style scoped>
.cart-modal {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  z-index: 1000;
  display: flex;
  align-items: center;
  justify-content: flex-end;
}

.cart-overlay {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  cursor: pointer;
}

.cart-panel {
  position: relative;
  background: white;
  width: 100%;
  max-width: 400px;
  height: 100vh;
  display: flex;
  flex-direction: column;
  box-shadow: -5px 0 20px rgba(0, 0, 0, 0.3);
  animation: slideIn 0.3s ease;
}

@keyframes slideIn {
  from {
    transform: translateX(100%);
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
  color: #999;
  transition: color 0.3s ease;
}

.close-btn:hover {
  color: #333;
}

.cart-content {
  flex: 1;
  overflow-y: auto;
  padding: 1rem;
}

.empty-cart {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  height: 100%;
  text-align: center;
  color: #999;
}

.empty-cart p {
  font-size: 1.1rem;
  margin: 0;
}

.empty-subtitle {
  font-size: 0.9rem;
  color: #bbb;
  margin-top: 0.5rem;
}

.cart-items {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.cart-item {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  background: #f9f9f9;
  border-radius: 8px;
  align-items: center;
}

.item-image {
  width: 80px;
  height: 80px;
  object-fit: cover;
  border-radius: 4px;
}

.item-info {
  flex: 1;
}

.item-info h3 {
  margin: 0;
  font-size: 0.95rem;
  color: #333;
  line-height: 1.2;
}

.item-artist {
  margin: 0.25rem 0;
  font-size: 0.85rem;
  color: #666;
}

.item-price {
  margin: 0.5rem 0 0 0;
  font-size: 0.9rem;
  font-weight: bold;
  color: #667eea;
}

.item-quantity {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 0.25rem;
}

.qty-btn {
  background: none;
  border: none;
  width: 24px;
  height: 24px;
  cursor: pointer;
  color: #667eea;
  font-weight: bold;
  transition: background 0.2s ease;
}

.qty-btn:hover {
  background: #f0f0f0;
}

.qty-value {
  width: 20px;
  text-align: center;
  font-weight: 600;
  color: #333;
}

.remove-btn {
  background: none;
  border: none;
  font-size: 1.1rem;
  cursor: pointer;
  transition: opacity 0.2s ease;
  padding: 0.25rem;
}

.remove-btn:hover {
  opacity: 0.6;
}

.cart-footer {
  border-top: 1px solid #eee;
  padding: 1.5rem;
  background: #f9f9f9;
}

.cart-total {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  font-size: 1.1rem;
  font-weight: bold;
  color: #333;
}

.total-price {
  color: #667eea;
  font-size: 1.3rem;
}

.checkout-btn {
  width: 100%;
  padding: 0.75rem;
  background: #667eea;
  color: white;
  border: none;
  border-radius: 8px;
  font-size: 1rem;
  font-weight: 600;
  cursor: pointer;
  transition: background 0.3s ease;
}

.checkout-btn:hover {
  background: #5a6fd8;
}

@media (max-width: 600px) {
  .cart-panel {
    max-width: 100%;
  }

  .cart-item {
    flex-wrap: wrap;
  }

  .item-info {
    width: 100%;
  }
}
</style>
