<template>
  <div class="cart-panel" :class="{ 'cart-open': isOpen }">
    <div class="cart-header">
      <h2>Shopping Cart</h2>
      <button class="close-btn" @click="$emit('close')" aria-label="Close cart">✕</button>
    </div>

    <div v-if="cartItems.length === 0" class="empty-cart">
      <p>Your cart is empty</p>
      <p class="subtitle">Start adding albums to your cart!</p>
    </div>

    <div v-else class="cart-content">
      <div class="cart-items">
        <div v-for="item in cartItems" :key="item.album.id" class="cart-item">
          <img :src="item.album.image_url" :alt="item.album.title" class="item-image" />
          <div class="item-details">
            <h3>{{ item.album.title }}</h3>
            <p class="artist-name">{{ item.album.artist.name }}</p>
            <p class="price">${{ item.album.price.toFixed(2) }}</p>
          </div>
          <div class="item-controls">
            <button @click="decrementQuantity(item.album.id)" class="qty-btn" aria-label="Decrease quantity">−</button>
            <span class="quantity">{{ item.quantity }}</span>
            <button @click="incrementQuantity(item.album.id)" class="qty-btn" aria-label="Increase quantity">+</button>
          </div>
          <button @click="removeFromCart(item.album.id)" class="remove-btn" aria-label="Remove from cart">🗑️</button>
        </div>
      </div>

      <div class="cart-footer">
        <div class="total-section">
          <label>Total:</label>
          <span class="total-price">${{ totalPrice.toFixed(2) }}</span>
        </div>
        <button @click="handleCheckout" class="checkout-btn">Proceed to Checkout</button>
        <button @click="clearCart" class="clear-btn">Clear Cart</button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useCart } from '../stores/useCart'

interface Props {
  isOpen: boolean
}

defineProps<Props>()

defineEmits<{
  close: []
}>()

const { cartItems, totalPrice, removeFromCart, incrementQuantity, decrementQuantity, clearCart } = useCart()

const handleCheckout = () => {
  alert('Checkout functionality coming soon!')
}
</script>

<style scoped>
.cart-panel {
  position: fixed;
  right: -400px;
  top: 0;
  width: 400px;
  height: 100vh;
  background: white;
  box-shadow: -4px 0 12px rgba(0, 0, 0, 0.2);
  transition: right 0.3s ease;
  z-index: 1000;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.cart-panel.cart-open {
  right: 0;
}

.cart-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem;
  border-bottom: 1px solid #e0e0e0;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
}

.cart-header h2 {
  margin: 0;
  font-size: 1.5rem;
}

.close-btn {
  background: none;
  border: none;
  color: white;
  font-size: 1.5rem;
  cursor: pointer;
  padding: 0;
  width: 32px;
  height: 32px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 4px;
  transition: background 0.3s;
}

.close-btn:hover {
  background: rgba(255, 255, 255, 0.2);
}

.empty-cart {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  height: 100%;
  color: #666;
}

.empty-cart p {
  font-size: 1.1rem;
  margin: 0.5rem 0;
}

.empty-cart .subtitle {
  font-size: 0.95rem;
  color: #999;
}

.cart-content {
  display: flex;
  flex-direction: column;
  height: 100%;
}

.cart-items {
  flex: 1;
  overflow-y: auto;
  padding: 1rem;
}

.cart-item {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  border: 1px solid #e0e0e0;
  border-radius: 8px;
  margin-bottom: 1rem;
  align-items: center;
}

.item-image {
  width: 80px;
  height: 80px;
  object-fit: cover;
  border-radius: 4px;
  flex-shrink: 0;
}

.item-details {
  flex: 1;
  min-width: 0;
}

.item-details h3 {
  margin: 0 0 0.25rem 0;
  font-size: 0.95rem;
  font-weight: 600;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.artist-name {
  margin: 0;
  font-size: 0.85rem;
  color: #666;
}

.price {
  margin: 0.25rem 0 0 0;
  font-weight: 600;
  color: #667eea;
}

.item-controls {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  flex-shrink: 0;
}

.qty-btn {
  width: 28px;
  height: 28px;
  border: 1px solid #ddd;
  background: white;
  border-radius: 4px;
  cursor: pointer;
  font-weight: bold;
  transition: all 0.2s;
}

.qty-btn:hover {
  background: #f0f0f0;
  border-color: #667eea;
}

.quantity {
  min-width: 20px;
  text-align: center;
  font-weight: 600;
}

.remove-btn {
  background: none;
  border: none;
  cursor: pointer;
  font-size: 1rem;
  flex-shrink: 0;
  padding: 0.25rem;
  border-radius: 4px;
  transition: background 0.2s;
}

.remove-btn:hover {
  background: #ffe0e0;
}

.cart-footer {
  padding: 1rem;
  border-top: 1px solid #e0e0e0;
  background: #f9f9f9;
}

.total-section {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  padding-bottom: 1rem;
  border-bottom: 1px solid #e0e0e0;
}

.total-section label {
  font-weight: 600;
  font-size: 1.1rem;
}

.total-price {
  font-weight: 700;
  font-size: 1.3rem;
  color: #667eea;
}

.checkout-btn {
  width: 100%;
  padding: 0.75rem;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  border: none;
  border-radius: 6px;
  font-weight: 600;
  cursor: pointer;
  margin-bottom: 0.5rem;
  transition: transform 0.2s;
}

.checkout-btn:hover {
  transform: translateY(-2px);
}

.clear-btn {
  width: 100%;
  padding: 0.75rem;
  background: white;
  color: #666;
  border: 1px solid #ddd;
  border-radius: 6px;
  cursor: pointer;
  transition: all 0.2s;
}

.clear-btn:hover {
  background: #f5f5f5;
  border-color: #999;
}

@media (max-width: 600px) {
  .cart-panel {
    width: 100%;
    right: -100%;
  }
}
</style>
