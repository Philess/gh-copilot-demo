<template>
  <Transition name="drawer">
    <div v-if="isOpen" class="drawer-overlay" @click.self="close">
      <div class="drawer" @click.stop>
        <div class="drawer-header">
          <h2>{{ $t('cart.title') }}</h2>
          <button class="close-btn" @click="close" :aria-label="$t('cart.close')">
            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
              <line x1="18" y1="6" x2="6" y2="18"></line>
              <line x1="6" y1="6" x2="18" y2="18"></line>
            </svg>
          </button>
        </div>

        <div class="drawer-content">
          <div v-if="isEmpty" class="empty-cart">
            <svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1" stroke-linecap="round" stroke-linejoin="round">
              <circle cx="9" cy="21" r="1"></circle>
              <circle cx="20" cy="21" r="1"></circle>
              <path d="M1 1h4l2.68 13.39a2 2 0 0 0 2 1.61h9.72a2 2 0 0 0 2-1.61L23 6H6"></path>
            </svg>
            <p>{{ $t('cart.empty') }}</p>
          </div>

          <div v-else class="cart-items">
            <div v-for="item in itemsList" :key="item.album.id" class="cart-item">
              <img 
                :src="item.album.image_url" 
                :alt="item.album.title"
                @error="handleImageError"
                class="item-image"
              />
              <div class="item-info">
                <h3>{{ item.album.title }}</h3>
                <p class="item-artist">{{ item.album.artist }}</p>
                <div class="item-price">{{ $t('currency.symbol') }}{{ item.album.price.toFixed(2) }}</div>
              </div>
              <div class="item-actions">
                <div class="quantity-controls">
                  <button 
                    @click="decrementQuantity(item.album.id)" 
                    class="qty-btn"
                    :aria-label="'Decrease quantity'"
                  >-</button>
                  <span class="quantity">{{ item.quantity }}</span>
                  <button 
                    @click="incrementQuantity(item.album.id)" 
                    class="qty-btn"
                    :aria-label="'Increase quantity'"
                  >+</button>
                </div>
                <button 
                  @click="removeItem(item.album.id)" 
                  class="remove-btn"
                  :aria-label="$t('cart.remove')"
                >
                  {{ $t('cart.remove') }}
                </button>
              </div>
            </div>
          </div>
        </div>

        <div v-if="!isEmpty" class="drawer-footer">
          <div class="total">
            <span class="total-label">{{ $t('cart.total') }}:</span>
            <span class="total-amount">{{ $t('currency.symbol') }}{{ total.toFixed(2) }}</span>
          </div>
          <button @click="clearCart" class="clear-btn">
            {{ $t('cart.clear') }}
          </button>
        </div>
      </div>
    </div>
  </Transition>
</template>

<script setup lang="ts">
import { watch } from 'vue'
import type { CartItem } from '../composables/useCart'

interface Props {
  isOpen: boolean
  itemsList: CartItem[]
  total: number
  isEmpty: boolean
}

const props = defineProps<Props>()

const emit = defineEmits<{
  close: []
  remove: [albumId: number]
  updateQuantity: [albumId: number, quantity: number]
  clear: []
}>()

function close() {
  emit('close')
}

function removeItem(albumId: number) {
  emit('remove', albumId)
}

function incrementQuantity(albumId: number) {
  const item = props.itemsList.find(i => i.album.id === albumId)
  if (item) {
    emit('updateQuantity', albumId, item.quantity + 1)
  }
}

function decrementQuantity(albumId: number) {
  const item = props.itemsList.find(i => i.album.id === albumId)
  if (item) {
    emit('updateQuantity', albumId, item.quantity - 1)
  }
}

function clearCart() {
  if (confirm('Are you sure you want to clear your cart?')) {
    emit('clear')
  }
}

function handleImageError(event: Event) {
  const target = event.target as HTMLImageElement
  target.src = 'https://via.placeholder.com/80x80/667eea/white?text=Album'
}

// Close on Escape key
watch(() => props.isOpen, (isOpen) => {
  if (isOpen) {
    document.addEventListener('keydown', handleEscape)
  } else {
    document.removeEventListener('keydown', handleEscape)
  }
})

function handleEscape(event: KeyboardEvent) {
  if (event.key === 'Escape') {
    close()
  }
}
</script>

<style scoped>
.drawer-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.5);
  backdrop-filter: blur(4px);
  z-index: 1000;
  display: flex;
  justify-content: flex-end;
}

.drawer {
  width: 100%;
  max-width: 450px;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  box-shadow: -4px 0 20px rgba(0, 0, 0, 0.3);
  display: flex;
  flex-direction: column;
  color: white;
}

.drawer-header {
  padding: 1.5rem;
  border-bottom: 1px solid rgba(255, 255, 255, 0.2);
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.drawer-header h2 {
  margin: 0;
  font-size: 1.5rem;
}

.close-btn {
  background: rgba(255, 255, 255, 0.2);
  border: none;
  color: white;
  width: 36px;
  height: 36px;
  border-radius: 50%;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.3s ease;
}

.close-btn:hover {
  background: rgba(255, 255, 255, 0.3);
  transform: rotate(90deg);
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
  padding: 4rem 2rem;
  text-align: center;
  opacity: 0.7;
}

.empty-cart svg {
  margin-bottom: 1rem;
}

.empty-cart p {
  font-size: 1.1rem;
}

.cart-items {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.cart-item {
  background: rgba(255, 255, 255, 0.1);
  border-radius: 12px;
  padding: 1rem;
  display: flex;
  gap: 1rem;
  backdrop-filter: blur(10px);
  transition: all 0.3s ease;
}

.cart-item:hover {
  background: rgba(255, 255, 255, 0.15);
}

.item-image {
  width: 80px;
  height: 80px;
  object-fit: cover;
  border-radius: 8px;
}

.item-info {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.item-info h3 {
  margin: 0;
  font-size: 1rem;
  font-weight: 600;
}

.item-artist {
  margin: 0;
  font-size: 0.9rem;
  opacity: 0.8;
}

.item-price {
  font-size: 1.1rem;
  font-weight: bold;
  margin-top: 0.25rem;
}

.item-actions {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  align-items: flex-end;
}

.quantity-controls {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  background: rgba(255, 255, 255, 0.2);
  border-radius: 20px;
  padding: 0.25rem;
}

.qty-btn {
  background: rgba(255, 255, 255, 0.3);
  border: none;
  color: white;
  width: 24px;
  height: 24px;
  border-radius: 50%;
  cursor: pointer;
  font-size: 1rem;
  font-weight: bold;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.2s ease;
}

.qty-btn:hover {
  background: rgba(255, 255, 255, 0.5);
}

.quantity {
  min-width: 30px;
  text-align: center;
  font-weight: 600;
}

.remove-btn {
  background: rgba(255, 77, 87, 0.8);
  color: white;
  border: none;
  padding: 0.4rem 0.8rem;
  border-radius: 6px;
  cursor: pointer;
  font-size: 0.85rem;
  transition: all 0.3s ease;
}

.remove-btn:hover {
  background: rgba(255, 77, 87, 1);
  transform: translateY(-2px);
}

.drawer-footer {
  padding: 1.5rem;
  border-top: 1px solid rgba(255, 255, 255, 0.2);
  background: rgba(0, 0, 0, 0.2);
}

.total {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  font-size: 1.3rem;
  font-weight: bold;
}

.clear-btn {
  width: 100%;
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid white;
  padding: 0.75rem;
  border-radius: 8px;
  cursor: pointer;
  font-size: 1rem;
  font-weight: 600;
  transition: all 0.3s ease;
}

.clear-btn:hover {
  background: white;
  color: #667eea;
}

/* Drawer transition */
.drawer-enter-active,
.drawer-leave-active {
  transition: all 0.3s ease;
}

.drawer-enter-from .drawer,
.drawer-leave-to .drawer {
  transform: translateX(100%);
}

.drawer-enter-from,
.drawer-leave-to {
  opacity: 0;
}

@media (max-width: 768px) {
  .drawer {
    max-width: 100%;
  }
  
  .cart-item {
    flex-direction: column;
  }
  
  .item-actions {
    flex-direction: row;
    justify-content: space-between;
    align-items: center;
    width: 100%;
  }
}
</style>
