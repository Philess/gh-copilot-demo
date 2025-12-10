<template>
  <div class="cart-icon" @click="$emit('toggle')" :title="$t('cart.title')">
    <svg 
      xmlns="http://www.w3.org/2000/svg" 
      width="28" 
      height="28" 
      viewBox="0 0 24 24" 
      fill="none" 
      stroke="currentColor" 
      stroke-width="2" 
      stroke-linecap="round" 
      stroke-linejoin="round"
    >
      <circle cx="9" cy="21" r="1"></circle>
      <circle cx="20" cy="21" r="1"></circle>
      <path d="M1 1h4l2.68 13.39a2 2 0 0 0 2 1.61h9.72a2 2 0 0 0 2-1.61L23 6H6"></path>
    </svg>
    <span v-if="count > 0" class="badge">{{ count }}</span>
  </div>
</template>

<script setup lang="ts">
interface Props {
  count: number
}

defineProps<Props>()

defineEmits<{
  toggle: []
}>()
</script>

<style scoped>
.cart-icon {
  position: relative;
  cursor: pointer;
  padding: 0.5rem;
  color: white;
  transition: all 0.3s ease;
  display: flex;
  align-items: center;
  justify-content: center;
}

.cart-icon:hover {
  transform: scale(1.1);
  color: rgba(255, 255, 255, 0.8);
}

.badge {
  position: absolute;
  top: 0;
  right: 0;
  background: #ff4757;
  color: white;
  border-radius: 50%;
  width: 20px;
  height: 20px;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.7rem;
  font-weight: bold;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
}
</style>
