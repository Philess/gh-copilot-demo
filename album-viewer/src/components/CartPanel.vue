<template>
  <Transition name="cart-overlay">
    <div v-if="isCartOpen" class="cart-overlay" @click="closeCart">
      <Transition name="cart-panel">
        <div v-if="isCartOpen" class="cart-panel" @click.stop>
          <!-- Header -->
          <div class="cart-header">
            <h2>{{ t('cart.title') }}</h2>
            <button class="close-btn" @click="closeCart" :aria-label="t('cart.close')">
              <svg width="24" height="24" viewBox="0 0 24 24" fill="none">
                <path d="M18 6L6 18M6 6L18 18" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
              </svg>
            </button>
          </div>

          <!-- Cart Content -->
          <div class="cart-content">
            <!-- Empty State -->
            <div v-if="cartItems.length === 0" class="empty-cart">
              <svg class="empty-icon" viewBox="0 0 24 24" fill="none">
                <path d="M9 2L7.17 4H3C1.9 4 1 4.9 1 6V18C1 19.1 1.9 20 3 20H21C22.1 20 23 19.1 23 18V6C23 4.9 22.1 4 21 4H16.83L15 2H9ZM9 9C9 10.66 10.34 12 12 12C13.66 12 15 10.66 15 9H17C17 11.76 14.76 14 12 14C9.24 14 7 11.76 7 9H9Z" fill="currentColor" opacity="0.3"/>
              </svg>
              <p class="empty-title">{{ t('cart.empty') }}</p>
              <p class="empty-description">{{ t('cart.emptyDescription') }}</p>
            </div>

            <!-- Cart Items -->
            <div v-else class="cart-items">
              <TransitionGroup name="cart-item">
                <div
                  v-for="item in cartItems"
                  :key="item.album.id"
                  class="cart-item"
                >
                  <img
                    :src="item.album.image_url"
                    :alt="item.album.title"
                    class="item-image"
                    @error="handleImageError"
                  />
                  <div class="item-info">
                    <h3 class="item-title">{{ item.album.title }}</h3>
                    <p class="item-artist">{{ item.album.artist }}</p>
                    <p class="item-price">${{ item.album.price.toFixed(2) }}</p>
                  </div>
                  <button
                    class="remove-btn"
                    @click="handleRemove(item.album.id)"
                    :aria-label="t('cart.remove')"
                  >
                    <svg width="20" height="20" viewBox="0 0 24 24" fill="none">
                      <path d="M6 19C6 20.1 6.9 21 8 21H16C17.1 21 18 20.1 18 19V7H6V19ZM19 4H15.5L14.5 3H9.5L8.5 4H5V6H19V4Z" fill="currentColor"/>
                    </svg>
                  </button>
                </div>
              </TransitionGroup>
            </div>
          </div>

          <!-- Cart Footer -->
          <div v-if="cartItems.length > 0" class="cart-footer">
            <div class="cart-summary">
              <div class="summary-row">
                <span class="summary-label">{{ t('cart.total') }}:</span>
                <span class="summary-value">
                  {{ getCartItemCount.value }} {{ getCartItemCount.value === 1 ? t('cart.item') : t('cart.items') }}
                </span>
              </div>
              <div class="summary-row total">
                <span class="summary-label">{{ t('cart.total') }}:</span>
                <span class="summary-value price">${{ getCartTotal.value.toFixed(2) }}</span>
              </div>
            </div>
            <button class="clear-btn" @click="handleClearCart">
              {{ t('cart.clearCart') }}
            </button>
          </div>
        </div>
      </Transition>
    </div>
  </Transition>
</template>

<script setup lang="ts">
import { useI18n } from 'vue-i18n'
import { useCart } from '../composables/useCart'

const { t } = useI18n()
const {
  cartItems,
  isCartOpen,
  removeFromCart,
  clearCart,
  closeCart,
  getCartItemCount,
  getCartTotal
} = useCart()

const handleRemove = (albumId: number) => {
  removeFromCart(albumId)
}

const handleClearCart = () => {
  if (confirm(t('cart.clearCart') + '?')) {
    clearCart()
  }
}

const handleImageError = (event: Event) => {
  const target = event.target as HTMLImageElement
  target.src = 'https://via.placeholder.com/60x60/667eea/white?text=Album'
}
</script>

<style scoped>
/* Overlay */
.cart-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  z-index: 1000;
  backdrop-filter: blur(4px);
}

/* Panel */
.cart-panel {
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  width: 400px;
  max-width: 100%;
  background: white;
  box-shadow: -4px 0 20px rgba(0, 0, 0, 0.15);
  display: flex;
  flex-direction: column;
  z-index: 1001;
}

/* Header */
.cart-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem;
  border-bottom: 2px solid #f0f0f0;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
}

.cart-header h2 {
  margin: 0;
  font-size: 1.5rem;
}

.close-btn {
  background: rgba(255, 255, 255, 0.2);
  border: none;
  border-radius: 8px;
  padding: 0.5rem;
  cursor: pointer;
  color: white;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.3s ease;
}

.close-btn:hover {
  background: rgba(255, 255, 255, 0.3);
  transform: scale(1.1);
}

/* Content */
.cart-content {
  flex: 1;
  overflow-y: auto;
  padding: 1rem;
}

/* Empty State */
.empty-cart {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 4rem 2rem;
  text-align: center;
  color: #999;
}

.empty-icon {
  width: 80px;
  height: 80px;
  margin-bottom: 1rem;
  color: #ddd;
}

.empty-title {
  font-size: 1.2rem;
  font-weight: 600;
  color: #666;
  margin: 0 0 0.5rem 0;
}

.empty-description {
  font-size: 0.9rem;
  margin: 0;
}

/* Cart Items */
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
  border-radius: 12px;
  transition: all 0.3s ease;
}

.cart-item:hover {
  background: #f0f0f0;
  transform: translateX(-4px);
}

.item-image {
  width: 60px;
  height: 60px;
  object-fit: cover;
  border-radius: 8px;
  flex-shrink: 0;
}

.item-info {
  flex: 1;
  min-width: 0;
}

.item-title {
  font-size: 0.95rem;
  font-weight: 600;
  margin: 0 0 0.25rem 0;
  color: #333;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.item-artist {
  font-size: 0.85rem;
  color: #666;
  margin: 0 0 0.5rem 0;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.item-price {
  font-size: 0.9rem;
  font-weight: 700;
  color: #667eea;
  margin: 0;
}

.remove-btn {
  background: transparent;
  border: none;
  color: #ff4757;
  cursor: pointer;
  padding: 0.5rem;
  border-radius: 8px;
  transition: all 0.3s ease;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-shrink: 0;
}

.remove-btn:hover {
  background: rgba(255, 71, 87, 0.1);
  transform: scale(1.1);
}

/* Footer */
.cart-footer {
  border-top: 2px solid #f0f0f0;
  padding: 1.5rem;
  background: white;
}

.cart-summary {
  margin-bottom: 1rem;
}

.summary-row {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 0.5rem;
  font-size: 0.9rem;
  color: #666;
}

.summary-row.total {
  font-size: 1.2rem;
  font-weight: 700;
  color: #333;
  padding-top: 0.5rem;
  border-top: 1px solid #e0e0e0;
}

.summary-value.price {
  color: #667eea;
}

.clear-btn {
  width: 100%;
  padding: 0.75rem;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  border: none;
  border-radius: 8px;
  font-size: 0.95rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
}

.clear-btn:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
}

/* Transitions */
.cart-overlay-enter-active,
.cart-overlay-leave-active {
  transition: opacity 0.3s ease;
}

.cart-overlay-enter-from,
.cart-overlay-leave-to {
  opacity: 0;
}

.cart-panel-enter-active,
.cart-panel-leave-active {
  transition: transform 0.3s ease;
}

.cart-panel-enter-from,
.cart-panel-leave-to {
  transform: translateX(100%);
}

.cart-item-enter-active,
.cart-item-leave-active {
  transition: all 0.3s ease;
}

.cart-item-enter-from {
  opacity: 0;
  transform: translateX(20px);
}

.cart-item-leave-to {
  opacity: 0;
  transform: translateX(-20px);
}

.cart-item-move {
  transition: transform 0.3s ease;
}

/* Mobile Responsive */
@media (max-width: 768px) {
  .cart-panel {
    width: 100%;
  }

  .cart-header {
    padding: 1rem;
  }

  .cart-header h2 {
    font-size: 1.25rem;
  }

  .cart-content {
    padding: 0.75rem;
  }

  .cart-item {
    padding: 0.75rem;
  }

  .item-title {
    font-size: 0.9rem;
  }

  .item-artist {
    font-size: 0.8rem;
  }
}
</style>
