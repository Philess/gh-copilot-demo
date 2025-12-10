<template>
  <Teleport to="body">
    <Transition name="drawer">
      <div v-if="cartStore.isDrawerOpen" class="drawer-overlay" @click="cartStore.closeDrawer">
        <div class="drawer" @click.stop>
          <div class="drawer-header">
            <h2>🛒 Your Cart</h2>
            <button class="close-btn" @click="cartStore.closeDrawer" aria-label="Close cart">
              <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <line x1="18" y1="6" x2="6" y2="18"></line>
                <line x1="6" y1="6" x2="18" y2="18"></line>
              </svg>
            </button>
          </div>
          
          <div class="drawer-content">
            <div v-if="cartStore.items.length === 0" class="empty-cart">
              <div class="empty-icon">🎵</div>
              <h3>Your cart is empty</h3>
              <p>Add some amazing albums to get started!</p>
            </div>
            
            <TransitionGroup v-else name="list" tag="div" class="cart-items">
              <CartItem 
                v-for="item in cartStore.items" 
                :key="item.album.id" 
                :item="item" 
              />
            </TransitionGroup>
          </div>
          
          <div v-if="cartStore.items.length > 0" class="drawer-footer">
            <div class="total">
              <span class="total-label">Total:</span>
              <span class="total-price">${{ cartStore.totalPrice.toFixed(2) }}</span>
            </div>
            
            <div class="footer-actions">
              <button class="btn btn-secondary" @click="cartStore.clearCart">
                Clear Cart
              </button>
              <button class="btn btn-primary">
                Checkout
              </button>
            </div>
          </div>
        </div>
      </div>
    </Transition>
  </Teleport>
</template>

<script setup lang="ts">
import { useCartStore } from '../stores/cart'
import CartItem from './CartItem.vue'

const cartStore = useCartStore()
</script>

<style scoped>
.drawer-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  z-index: 1000;
  display: flex;
  justify-content: flex-end;
}

.drawer {
  width: 100%;
  max-width: 420px;
  height: 100%;
  background: white;
  display: flex;
  flex-direction: column;
  box-shadow: -10px 0 30px rgba(0, 0, 0, 0.2);
}

.drawer-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 1.5rem;
  border-bottom: 1px solid #eee;
}

.drawer-header h2 {
  margin: 0;
  font-size: 1.5rem;
  color: #333;
}

.close-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: #666;
  padding: 0.5rem;
  border-radius: 8px;
  transition: all 0.2s ease;
  display: flex;
  align-items: center;
  justify-content: center;
}

.close-btn:hover {
  background: #f1f3f4;
  color: #333;
}

.drawer-content {
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
  color: #666;
}

.empty-icon {
  font-size: 4rem;
  margin-bottom: 1rem;
  opacity: 0.5;
}

.empty-cart h3 {
  margin: 0 0 0.5rem 0;
  color: #333;
}

.empty-cart p {
  margin: 0;
  font-size: 0.9rem;
}

.cart-items {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
}

.drawer-footer {
  padding: 1.5rem;
  border-top: 1px solid #eee;
  background: #f8f9fa;
}

.total {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
}

.total-label {
  font-size: 1.1rem;
  color: #666;
}

.total-price {
  font-size: 1.5rem;
  font-weight: bold;
  color: #667eea;
}

.footer-actions {
  display: flex;
  gap: 0.75rem;
}

.btn {
  flex: 1;
  padding: 0.875rem;
  border: none;
  border-radius: 10px;
  font-size: 1rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
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
  color: #666;
  border: 2px solid #ddd;
}

.btn-secondary:hover {
  background: #f1f3f4;
  border-color: #ccc;
}

/* Drawer transition */
.drawer-enter-active,
.drawer-leave-active {
  transition: opacity 0.3s ease;
}

.drawer-enter-active .drawer,
.drawer-leave-active .drawer {
  transition: transform 0.3s ease;
}

.drawer-enter-from,
.drawer-leave-to {
  opacity: 0;
}

.drawer-enter-from .drawer,
.drawer-leave-to .drawer {
  transform: translateX(100%);
}

/* List transition */
.list-enter-active,
.list-leave-active {
  transition: all 0.3s ease;
}

.list-enter-from,
.list-leave-to {
  opacity: 0;
  transform: translateX(30px);
}

.list-move {
  transition: transform 0.3s ease;
}

@media (max-width: 480px) {
  .drawer {
    max-width: 100%;
  }
}
</style>
