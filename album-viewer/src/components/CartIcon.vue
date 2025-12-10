<template>
  <div class="cart-icon" @click="toggleCart" :title="t.cart.title">
    <div class="icon-wrapper">
      <svg 
        class="cart-svg" 
        viewBox="0 0 24 24" 
        fill="none" 
        stroke="currentColor" 
        stroke-width="2"
      >
        <path d="M9 2L7 6M17 2L19 6M3 6h18M5 6h14l-1 13H6L5 6z"/>
        <circle cx="9" cy="21" r="1"/>
        <circle cx="17" cy="21" r="1"/>
      </svg>
      <span v-if="cartCount > 0" class="badge">{{ cartCount }}</span>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useCart } from '../composables/useCart'
import { useI18n } from '../i18n'

const { cartCount, toggleCart } = useCart()
const { t } = useI18n()
</script>

<style scoped>
.cart-icon {
  cursor: pointer;
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.3s ease;
}

.icon-wrapper {
  position: relative;
  padding: 0.5rem;
}

.cart-svg {
  width: 32px;
  height: 32px;
  color: white;
  transition: all 0.3s ease;
}

.cart-icon:hover .cart-svg {
  color: #ffffff;
  transform: scale(1.1);
}

.badge {
  position: absolute;
  top: 0;
  right: 0;
  background: #ff4757;
  color: white;
  font-size: 0.75rem;
  font-weight: bold;
  padding: 0.15rem 0.4rem;
  border-radius: 10px;
  min-width: 20px;
  text-align: center;
  animation: pulse 0.3s ease;
  box-shadow: 0 2px 8px rgba(255, 71, 87, 0.4);
}

@keyframes pulse {
  0% {
    transform: scale(0.8);
  }
  50% {
    transform: scale(1.2);
  }
  100% {
    transform: scale(1);
  }
}

@media (max-width: 768px) {
  .cart-svg {
    width: 28px;
    height: 28px;
  }
  
  .badge {
    font-size: 0.7rem;
    padding: 0.1rem 0.35rem;
  }
}
</style>
