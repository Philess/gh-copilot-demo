<template>
  <div class="cart-item">
    <div class="item-image">
      <img 
        :src="item.album.image_url" 
        :alt="item.album.title"
        @error="handleImageError"
      />
    </div>
    
    <div class="item-details">
      <h4 class="item-title">{{ item.album.title }}</h4>
      <p class="item-artist">{{ item.album.artist }}</p>
      <p class="item-price">${{ item.album.price.toFixed(2) }}</p>
    </div>
    
    <button 
      class="remove-btn" 
      @click="handleRemove"
      :title="t.cart.removeFromCart"
    >
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
        <path d="M18 6L6 18M6 6l12 12"/>
      </svg>
    </button>
  </div>
</template>

<script setup lang="ts">
import type { CartItem } from '../types/cart'
import { useCart } from '../composables/useCart'
import { useI18n } from '../i18n'

interface Props {
  item: CartItem
}

const props = defineProps<Props>()
const { removeFromCart } = useCart()
const { t } = useI18n()

const handleRemove = () => {
  removeFromCart(props.item.album.id)
}

const handleImageError = (event: Event): void => {
  const target = event.target as HTMLImageElement
  target.src = 'https://via.placeholder.com/80x80/667eea/white?text=Album'
}
</script>

<style scoped>
.cart-item {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  background: rgba(255, 255, 255, 0.05);
  border-radius: 8px;
  transition: all 0.3s ease;
  align-items: center;
}

.cart-item:hover {
  background: rgba(255, 255, 255, 0.1);
}

.item-image {
  flex-shrink: 0;
}

.item-image img {
  width: 60px;
  height: 60px;
  border-radius: 6px;
  object-fit: cover;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
}

.item-details {
  flex: 1;
  min-width: 0;
}

.item-title {
  font-size: 0.95rem;
  font-weight: 600;
  color: white;
  margin: 0 0 0.25rem 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.item-artist {
  font-size: 0.85rem;
  color: rgba(255, 255, 255, 0.7);
  margin: 0 0 0.5rem 0;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.item-price {
  font-size: 1rem;
  font-weight: bold;
  color: #ffd93d;
  margin: 0;
}

.remove-btn {
  flex-shrink: 0;
  width: 32px;
  height: 32px;
  background: rgba(255, 71, 87, 0.2);
  border: none;
  border-radius: 6px;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.3s ease;
  padding: 0;
}

.remove-btn svg {
  width: 18px;
  height: 18px;
  color: #ff4757;
}

.remove-btn:hover {
  background: #ff4757;
  transform: scale(1.1);
}

.remove-btn:hover svg {
  color: white;
}

@media (max-width: 768px) {
  .cart-item {
    padding: 0.75rem;
    gap: 0.75rem;
  }
  
  .item-image img {
    width: 50px;
    height: 50px;
  }
  
  .item-title {
    font-size: 0.9rem;
  }
  
  .item-artist {
    font-size: 0.8rem;
  }
  
  .item-price {
    font-size: 0.9rem;
  }
}
</style>
