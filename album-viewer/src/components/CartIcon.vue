<template>
  <div class="cart-icon-wrapper">
    <button 
      class="cart-icon"
      @click="toggleCart"
      :class="{ 'has-items': itemCount > 0 }"
      :title="t.cart.title"
    >
      <span class="icon">🛒</span>
      <span v-if="itemCount > 0" class="badge">{{ itemCount }}</span>
    </button>
  </div>
</template>

<script setup lang="ts">
import { useI18n } from '../composables/useI18n'
import { useCart } from '../composables/useCart'

defineProps<{
  isOpen: boolean
}>()

const emit = defineEmits<{
  toggle: []
}>()

const { t } = useI18n()
const { itemCount } = useCart()

const toggleCart = () => {
  emit('toggle')
}
</script>

<style scoped>
.cart-icon-wrapper {
  position: relative;
}

.cart-icon {
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid rgba(255, 255, 255, 0.3);
  padding: 0.5rem 0.75rem;
  border-radius: 8px;
  font-size: 1.5rem;
  cursor: pointer;
  transition: all 0.3s ease;
  display: flex;
  align-items: center;
  justify-content: center;
  position: relative;
  min-width: 50px;
  height: 50px;
  backdrop-filter: blur(10px);
}

.cart-icon:hover {
  background: rgba(255, 255, 255, 0.3);
  border-color: rgba(255, 255, 255, 0.5);
  transform: scale(1.05);
}

.cart-icon.has-items {
  background: rgba(102, 126, 234, 0.3);
  border-color: rgba(102, 126, 234, 0.6);
}

.icon {
  display: flex;
  align-items: center;
  justify-content: center;
}

.badge {
  position: absolute;
  top: -8px;
  right: -8px;
  background: #667eea;
  color: white;
  border-radius: 50%;
  width: 24px;
  height: 24px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.75rem;
  font-weight: bold;
  border: 2px solid white;
  animation: badgePulse 0.3s ease;
}

@keyframes badgePulse {
  0% {
    transform: scale(1.3);
    opacity: 0.8;
  }
  100% {
    transform: scale(1);
    opacity: 1;
  }
}

@media (max-width: 768px) {
  .cart-icon {
    padding: 0.5rem;
    min-width: auto;
  }
}
</style>
