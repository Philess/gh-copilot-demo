<template>
  <div class="cart-icon" @click="toggleCart">
    <div class="cart-button">
      <svg class="cart-svg" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M9 2L7.17 4H3C1.9 4 1 4.9 1 6V18C1 19.1 1.9 20 3 20H21C22.1 20 23 19.1 23 18V6C23 4.9 22.1 4 21 4H16.83L15 2H9ZM9 9C9 10.66 10.34 12 12 12C13.66 12 15 10.66 15 9H17C17 11.76 14.76 14 12 14C9.24 14 7 11.76 7 9H9Z" fill="currentColor"/>
      </svg>
      <span v-if="itemCount > 0" class="cart-badge">{{ itemCount }}</span>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useCart } from '../composables/useCart'

const { getCartItemCount, toggleCart } = useCart()
const itemCount = getCartItemCount
</script>

<style scoped>
.cart-icon {
  position: relative;
  cursor: pointer;
  transition: transform 0.2s ease;
}

.cart-icon:hover {
  transform: scale(1.1);
}

.cart-button {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0.5rem;
  border-radius: 8px;
  background: rgba(255, 255, 255, 0.1);
  border: 2px solid rgba(255, 255, 255, 0.3);
  transition: all 0.3s ease;
  backdrop-filter: blur(10px);
}

.cart-button:hover {
  background: rgba(255, 255, 255, 0.2);
  border-color: rgba(255, 255, 255, 0.5);
}

.cart-svg {
  width: 24px;
  height: 24px;
  color: white;
}

.cart-badge {
  position: absolute;
  top: -8px;
  right: -8px;
  background: #ff4757;
  color: white;
  border-radius: 50%;
  width: 22px;
  height: 22px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.75rem;
  font-weight: bold;
  border: 2px solid white;
  animation: bounce 0.5s ease;
}

@keyframes bounce {
  0%, 100% {
    transform: scale(1);
  }
  50% {
    transform: scale(1.2);
  }
}

@media (max-width: 768px) {
  .cart-button {
    padding: 0.4rem;
  }
  
  .cart-svg {
    width: 20px;
    height: 20px;
  }
  
  .cart-badge {
    width: 18px;
    height: 18px;
    font-size: 0.65rem;
    top: -6px;
    right: -6px;
  }
}
</style>
