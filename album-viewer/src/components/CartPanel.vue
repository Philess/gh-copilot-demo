<template>
  <div class="cart-panel" v-if="open">
    <div class="cart-header">
      <h2>{{ t('cart.title') || 'Cart' }}</h2>
      <button class="close" @click="$emit('close')">×</button>
    </div>
    <div v-if="items.length === 0" class="empty">{{ t('cart.empty') || 'Your cart is empty' }}</div>
    <ul v-else class="cart-list">
      <li v-for="item in items" :key="item.album.id" class="cart-item">
        <div class="info">
          <div class="title">{{ item.album.title }}</div>
          <div class="meta">{{ item.album.artist }} • ${{ item.album.price.toFixed(2) }}</div>
        </div>
        <div class="qty">{{ t('cart.quantity') || 'Qty' }}: {{ item.qty }}</div>
        <div class="line-total">${{ (item.album.price * item.qty).toFixed(2) }}</div>
        <button class="remove" @click="remove(item.album.id)">{{ t('cart.remove') || 'Remove' }}</button>
      </li>
    </ul>
    <div class="cart-footer" v-if="items.length">
      <div class="subtotal">{{ t('cart.total') || 'Total' }}: ${{ subtotal.toFixed(2) }}</div>
      <div class="actions">
        <button class="clear" @click="clear()">{{ t('cart.clear') || 'Clear Cart' }}</button>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useI18n } from 'vue-i18n'
import { useCart } from '../composables/useCart'

interface Props { open: boolean }

defineProps<Props>()

defineEmits<{ (e: 'close'): void }>()

const { t } = useI18n()
const { cartItems: items, subtotal, remove, clear } = useCart()
</script>

<style scoped>
.cart-panel {
  position: fixed;
  right: 1rem;
  top: 1rem;
  width: 360px;
  max-height: 80vh;
  background: white;
  border-radius: 12px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.2);
  overflow: hidden;
  display: flex;
  flex-direction: column;
}
.cart-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 1.25rem;
  border-bottom: 1px solid #eee;
}
.close { border: none; background: transparent; font-size: 1.5rem; cursor: pointer; }
.empty { padding: 1rem 1.25rem; color: #666; }
.cart-list { list-style: none; margin: 0; padding: 0; overflow: auto; }
.cart-item { display: grid; grid-template-columns: 1fr auto auto auto; gap: 0.75rem; align-items: center; padding: 0.75rem 1.25rem; border-bottom: 1px solid #f4f4f4; }
.info .title { font-weight: 600; }
.meta { color: #666; font-size: 0.9rem; }
.qty, .line-total { font-weight: 600; }
.remove { border: 1px solid #e33; color: #e33; background: transparent; padding: 0.4rem 0.6rem; border-radius: 6px; cursor: pointer; }
.cart-footer { padding: 1rem 1.25rem; display: flex; justify-content: space-between; align-items: center; }
.clear { background: #eee; border: none; padding: 0.5rem 0.75rem; border-radius: 6px; cursor: pointer; }
@media (max-width: 768px) { .cart-panel { left: 1rem; right: 1rem; width: auto; } }
</style>
