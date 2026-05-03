<template>
  <Teleport to="body">
    <Transition name="modal">
      <div v-if="isOpen" class="modal-overlay" @click="onClose">
        <div class="modal-content" @click.stop>
          <div class="modal-header">
            <h2>🛒 Shopping Cart</h2>
            <button class="close-btn" @click="onClose" aria-label="Close cart">×</button>
          </div>

          <div v-if="cartItems.length === 0" class="empty-cart">
            <p>Your cart is empty</p>
            <button class="btn btn-primary" @click="onClose">Continue Shopping</button>
          </div>

          <div v-else class="cart-body">
            <div class="cart-items">
              <div v-for="item in cartItems" :key="item.id" class="cart-item">
                <img :src="item.image_url" :alt="item.title" class="cart-item-image" />
                <div class="cart-item-info">
                  <h3>{{ item.title }}</h3>
                  <p>{{ item.artist }}</p>
                  <p class="item-price">${{ item.price.toFixed(2) }}</p>
                </div>
                <div class="cart-item-controls">
                  <div class="quantity-controls">
                    <button @click="updateQuantity(item.id, item.quantity - 1)" class="qty-btn">-</button>
                    <span class="quantity">{{ item.quantity }}</span>
                    <button @click="updateQuantity(item.id, item.quantity + 1)" class="qty-btn">+</button>
                  </div>
                  <button @click="removeFromCart(item.id)" class="remove-btn">Remove</button>
                </div>
              </div>
            </div>

            <div class="cart-footer">
              <div class="cart-total">
                <span class="total-label">Total:</span>
                <span class="total-amount">${{ cartTotal.toFixed(2) }}</span>
              </div>
              <div class="cart-actions">
                <button class="btn btn-secondary" @click="clearCart">Clear Cart</button>
                <button class="btn btn-primary">Checkout</button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </Transition>
  </Teleport>
</template>

<script setup lang="ts">
import { useCart } from '../composables/useCart'

interface Props {
  isOpen: boolean
}

defineProps<Props>()

const emit = defineEmits<{
  close: []
}>()

const { cartItems, cartTotal, removeFromCart, updateQuantity, clearCart } = useCart()

const onClose = () => {
  emit('close')
}
</script>

<style scoped>
.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0, 0, 0, 0.7);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
  padding: 1rem;
}

.modal-content {
  background: white;
  border-radius: 20px;
  max-width: 600px;
  width: 100%;
  max-height: 80vh;
  display: flex;
  flex-direction: column;
  box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
}

.modal-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem;
  border-bottom: 1px solid #e0e0e0;
}

.modal-header h2 {
  margin: 0;
  color: #333;
  font-size: 1.5rem;
}

.close-btn {
  background: none;
  border: none;
  font-size: 2rem;
  color: #666;
  cursor: pointer;
  padding: 0;
  width: 40px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 50%;
  transition: all 0.3s ease;
}

.close-btn:hover {
  background: #f0f0f0;
  color: #333;
}

.empty-cart {
  padding: 3rem;
  text-align: center;
}

.empty-cart p {
  font-size: 1.2rem;
  color: #666;
  margin-bottom: 1.5rem;
}

.cart-body {
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.cart-items {
  flex: 1;
  overflow-y: auto;
  padding: 1rem;
  max-height: 50vh;
}

.cart-item {
  display: flex;
  gap: 1rem;
  padding: 1rem;
  border: 1px solid #e0e0e0;
  border-radius: 10px;
  margin-bottom: 1rem;
  background: #f9f9f9;
}

.cart-item-image {
  width: 80px;
  height: 80px;
  object-fit: cover;
  border-radius: 8px;
}

.cart-item-info {
  flex: 1;
}

.cart-item-info h3 {
  margin: 0 0 0.25rem 0;
  font-size: 1.1rem;
  color: #333;
}

.cart-item-info p {
  margin: 0;
  color: #666;
  font-size: 0.9rem;
}

.item-price {
  font-weight: bold;
  color: #667eea !important;
  margin-top: 0.5rem !important;
}

.cart-item-controls {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  align-items: flex-end;
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
  background: #667eea;
  color: white;
  border: none;
  width: 28px;
  height: 28px;
  border-radius: 6px;
  cursor: pointer;
  font-size: 1rem;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: all 0.2s ease;
}

.qty-btn:hover {
  background: #5568d3;
  transform: scale(1.1);
}

.quantity {
  min-width: 30px;
  text-align: center;
  font-weight: bold;
  color: #333;
}

.remove-btn {
  background: none;
  border: none;
  color: #e74c3c;
  font-size: 0.9rem;
  cursor: pointer;
  text-decoration: underline;
  padding: 0;
}

.remove-btn:hover {
  color: #c0392b;
}

.cart-footer {
  padding: 1.5rem;
  border-top: 1px solid #e0e0e0;
  background: #f9f9f9;
}

.cart-total {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  font-size: 1.3rem;
  font-weight: bold;
}

.total-label {
  color: #333;
}

.total-amount {
  color: #667eea;
  font-size: 1.5rem;
}

.cart-actions {
  display: flex;
  gap: 1rem;
}

.btn {
  flex: 1;
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 8px;
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
  background: #5568d3;
  transform: translateY(-2px);
  box-shadow: 0 5px 15px rgba(102, 126, 234, 0.4);
}

.btn-secondary {
  background: #e0e0e0;
  color: #333;
}

.btn-secondary:hover {
  background: #d0d0d0;
}

/* Modal transition */
.modal-enter-active,
.modal-leave-active {
  transition: opacity 0.3s ease;
}

.modal-enter-from,
.modal-leave-to {
  opacity: 0;
}

.modal-enter-active .modal-content,
.modal-leave-active .modal-content {
  transition: transform 0.3s ease;
}

.modal-enter-from .modal-content,
.modal-leave-to .modal-content {
  transform: scale(0.9);
}
</style>
