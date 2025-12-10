<template>
  <div class="cart-item">
    <img 
      :src="item.album.image_url" 
      :alt="item.album.title"
      class="item-image"
      @error="handleImageError"
    />
    
    <div class="item-details">
      <h4 class="item-title">{{ item.album.title }}</h4>
      <p class="item-artist">{{ item.album.artist }}</p>
      <p class="item-price">${{ item.album.price.toFixed(2) }}</p>
    </div>
    
    <div class="item-actions">
      <div class="quantity-controls">
        <button 
          class="qty-btn" 
          @click="decrementQuantity"
          aria-label="Decrease quantity"
        >
          −
        </button>
        <span class="quantity">{{ item.quantity }}</span>
        <button 
          class="qty-btn" 
          @click="incrementQuantity"
          aria-label="Increase quantity"
        >
          +
        </button>
      </div>
      
      <button 
        class="remove-btn" 
        @click="removeItem"
        aria-label="Remove from cart"
      >
        <svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
          <polyline points="3 6 5 6 21 6"></polyline>
          <path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path>
        </svg>
      </button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useCartStore } from '../stores/cart'
import type { CartItem } from '../types/cart'

interface Props {
  item: CartItem
}

const props = defineProps<Props>()
const cartStore = useCartStore()

const incrementQuantity = (): void => {
  cartStore.updateQuantity(props.item.album.id, props.item.quantity + 1)
}

const decrementQuantity = (): void => {
  cartStore.updateQuantity(props.item.album.id, props.item.quantity - 1)
}

const removeItem = (): void => {
  cartStore.removeFromCart(props.item.album.id)
}

const handleImageError = (event: Event): void => {
  const target = event.target as HTMLImageElement
  target.src = 'https://via.placeholder.com/60x60/667eea/white?text=Album'
}
</script>

<style scoped>
.cart-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  background: #f8f9fa;
  border-radius: 12px;
  transition: all 0.3s ease;
}

.cart-item:hover {
  background: #f1f3f4;
}

.item-image {
  width: 60px;
  height: 60px;
  border-radius: 8px;
  object-fit: cover;
  flex-shrink: 0;
}

.item-details {
  flex: 1;
  min-width: 0;
}

.item-title {
  font-size: 0.95rem;
  font-weight: 600;
  color: #333;
  margin: 0 0 0.25rem 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.item-artist {
  font-size: 0.8rem;
  color: #666;
  margin: 0 0 0.25rem 0;
}

.item-price {
  font-size: 0.9rem;
  font-weight: 600;
  color: #667eea;
  margin: 0;
}

.item-actions {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  gap: 0.5rem;
}

.quantity-controls {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  background: white;
  border-radius: 8px;
  padding: 0.25rem;
}

.qty-btn {
  width: 28px;
  height: 28px;
  border: none;
  background: #667eea;
  color: white;
  border-radius: 6px;
  cursor: pointer;
  font-size: 1rem;
  font-weight: bold;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.2s ease;
}

.qty-btn:hover {
  background: #5a6fd8;
  transform: scale(1.05);
}

.quantity {
  min-width: 24px;
  text-align: center;
  font-weight: 600;
  color: #333;
}

.remove-btn {
  background: none;
  border: none;
  color: #ff4757;
  cursor: pointer;
  padding: 0.25rem;
  border-radius: 6px;
  transition: all 0.2s ease;
  display: flex;
  align-items: center;
  justify-content: center;
}

.remove-btn:hover {
  background: rgba(255, 71, 87, 0.1);
  transform: scale(1.1);
}
</style>
