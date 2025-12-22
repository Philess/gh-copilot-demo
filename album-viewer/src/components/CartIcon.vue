<template>
  <button class="cart-icon" @click="$emit('open')" aria-label="Open cart">
    <svg width="24" height="24" viewBox="0 0 24 24" fill="none" aria-hidden>
      <path d="M6 6h15l-1.5 9h-12L4 3H2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
      <circle cx="10" cy="20" r="1" fill="currentColor" />
      <circle cx="18" cy="20" r="1" fill="currentColor" />
    </svg>
    <span v-if="count > 0" class="badge" aria-live="polite">{{ count }}</span>
  </button>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { useCart } from '../composables/useCart'

const { count } = useCart()
</script>

<style scoped>
.cart-icon {
  position: relative;
  background: transparent;
  border: none;
  color: white;
  cursor: pointer;
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.25rem 0.5rem;
}

.badge {
  position: absolute;
  top: -6px;
  right: -6px;
  background: #ff5252;
  color: white;
  font-weight: 700;
  font-size: 0.75rem;
  line-height: 1;
  border-radius: 999px;
  padding: 0.2rem 0.45rem;
  min-width: 18px;
  text-align: center;
}
</style>