<template>
  <Transition name="backdrop">
    <div 
      v-if="isCartOpen" 
      class="cart-backdrop" 
      @click="closeCart"
    ></div>
  </Transition>
  
  <Transition name="slide">
    <div v-if="isCartOpen" class="cart-sidebar">
      <div class="cart-header">
        <h2>{{ t.cart.title }}</h2>
        <button 
          class="close-btn" 
          @click="closeCart"
          :title="t.cart.close"
        >
          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            <path d="M18 6L6 18M6 6l12 12"/>
          </svg>
        </button>
      </div>

      <div class="cart-content">
        <div v-if="cartItems.length === 0" class="empty-cart">
          <svg class="empty-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            <path d="M9 2L7 6M17 2L19 6M3 6h18M5 6h14l-1 13H6L5 6z"/>
            <circle cx="9" cy="21" r="1"/>
            <circle cx="17" cy="21" r="1"/>
          </svg>
          <h3>{{ t.cart.empty }}</h3>
          <p>{{ t.cart.emptyDescription }}</p>
        </div>

        <div v-else class="cart-items">
          <CartItem 
            v-for="item in cartItems" 
            :key="item.album.id"
            :item="item"
          />
        </div>
      </div>

      <div v-if="cartItems.length > 0" class="cart-footer">
        <div class="cart-summary">
          <div class="summary-row">
            <span class="summary-label">{{ t.cart.itemCount.replace('{count}', cartCount.toString()) }}</span>
          </div>
          <div class="summary-row total">
            <span class="summary-label">{{ t.cart.total }}:</span>
            <span class="summary-value">${{ cartTotal.toFixed(2) }}</span>
          </div>
        </div>
        
        <button 
          class="clear-btn" 
          @click="handleClearCart"
          :title="t.cart.clear"
        >
          {{ t.cart.clear }}
        </button>
      </div>
    </div>
  </Transition>
</template>

<script setup lang="ts">
import { useCart } from '../composables/useCart'
import { useI18n } from '../i18n'
import CartItem from './CartItem.vue'

const { cartItems, cartCount, cartTotal, isCartOpen, closeCart, clearCart } = useCart()
const { t } = useI18n()

const handleClearCart = () => {
  if (confirm('Are you sure you want to clear your cart?')) {
    clearCart()
  }
}
</script>

<style scoped>
.cart-backdrop {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  z-index: 998;
  backdrop-filter: blur(2px);
}

.cart-sidebar {
  position: fixed;
  top: 0;
  right: 0;
  width: 420px;
  max-width: 90vw;
  height: 100vh;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  box-shadow: -4px 0 20px rgba(0, 0, 0, 0.3);
  z-index: 999;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.cart-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem;
  background: rgba(0, 0, 0, 0.2);
  backdrop-filter: blur(10px);
  border-bottom: 1px solid rgba(255, 255, 255, 0.1);
}

.cart-header h2 {
  color: white;
  margin: 0;
  font-size: 1.5rem;
}

.close-btn {
  width: 36px;
  height: 36px;
  background: rgba(255, 255, 255, 0.1);
  border: none;
  border-radius: 8px;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.3s ease;
}

.close-btn svg {
  width: 20px;
  height: 20px;
  color: white;
}

.close-btn:hover {
  background: rgba(255, 255, 255, 0.2);
  transform: scale(1.1);
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
  color: white;
  padding: 2rem;
}

.empty-icon {
  width: 80px;
  height: 80px;
  opacity: 0.5;
  margin-bottom: 1rem;
}

.empty-cart h3 {
  font-size: 1.5rem;
  margin: 0 0 0.5rem 0;
}

.empty-cart p {
  font-size: 1rem;
  opacity: 0.8;
  margin: 0;
}

.cart-items {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
}

.cart-footer {
  padding: 1.5rem;
  background: rgba(0, 0, 0, 0.2);
  backdrop-filter: blur(10px);
  border-top: 1px solid rgba(255, 255, 255, 0.1);
}

.cart-summary {
  margin-bottom: 1rem;
}

.summary-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  color: white;
  margin-bottom: 0.5rem;
}

.summary-row.total {
  margin-top: 1rem;
  padding-top: 1rem;
  border-top: 1px solid rgba(255, 255, 255, 0.2);
  font-size: 1.2rem;
  font-weight: bold;
}

.summary-label {
  font-size: 0.95rem;
}

.summary-value {
  font-size: 1.2rem;
  color: #ffd93d;
}

.clear-btn {
  width: 100%;
  padding: 0.75rem;
  background: rgba(255, 71, 87, 0.2);
  color: #ff4757;
  border: 2px solid #ff4757;
  border-radius: 8px;
  font-size: 1rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.clear-btn:hover {
  background: #ff4757;
  color: white;
  transform: translateY(-2px);
}

/* Animations */
.backdrop-enter-active,
.backdrop-leave-active {
  transition: opacity 0.3s ease;
}

.backdrop-enter-from,
.backdrop-leave-to {
  opacity: 0;
}

.slide-enter-active,
.slide-leave-active {
  transition: transform 0.3s ease;
}

.slide-enter-from,
.slide-leave-to {
  transform: translateX(100%);
}

/* Scrollbar styling */
.cart-content::-webkit-scrollbar {
  width: 8px;
}

.cart-content::-webkit-scrollbar-track {
  background: rgba(0, 0, 0, 0.1);
}

.cart-content::-webkit-scrollbar-thumb {
  background: rgba(255, 255, 255, 0.3);
  border-radius: 4px;
}

.cart-content::-webkit-scrollbar-thumb:hover {
  background: rgba(255, 255, 255, 0.5);
}

@media (max-width: 768px) {
  .cart-sidebar {
    width: 100%;
    max-width: 100vw;
  }
  
  .cart-header {
    padding: 1rem;
  }
  
  .cart-header h2 {
    font-size: 1.3rem;
  }
  
  .cart-footer {
    padding: 1rem;
  }
}
</style>
