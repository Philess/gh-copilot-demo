<template>
  <div class="language-selector">
    <label class="selector-label">{{ t.language.label }}:</label>
    <div class="language-buttons">
      <button
        v-for="lang in languages"
        :key="lang.code"
        @click="setLocale(lang.code)"
        :class="['lang-btn', { active: locale === lang.code }]"
        :title="lang.name"
      >
        <span class="flag">{{ lang.flag }}</span>
        <span class="lang-code">{{ lang.code.toUpperCase() }}</span>
      </button>
    </div>
  </div>
</template>

<script setup lang="ts">
import { useI18n } from '../i18n'

const { t, locale, setLocale, languages } = useI18n()
</script>

<style scoped>
.language-selector {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.selector-label {
  color: white;
  font-size: 1rem;
  font-weight: 500;
}

.language-buttons {
  display: flex;
  gap: 0.5rem;
}

.lang-btn {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.5rem 1rem;
  background: rgba(255, 255, 255, 0.15);
  border: 2px solid rgba(255, 255, 255, 0.3);
  border-radius: 8px;
  color: white;
  font-size: 0.9rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.3s ease;
  backdrop-filter: blur(10px);
}

.lang-btn:hover {
  background: rgba(255, 255, 255, 0.25);
  border-color: rgba(255, 255, 255, 0.5);
  transform: translateY(-2px);
}

.lang-btn.active {
  background: white;
  color: #667eea;
  border-color: white;
}

.flag {
  font-size: 1.2rem;
}

.lang-code {
  font-size: 0.85rem;
}

@media (max-width: 768px) {
  .language-selector {
    flex-direction: column;
    align-items: flex-start;
    gap: 0.5rem;
  }
  
  .selector-label {
    font-size: 0.9rem;
  }
  
  .lang-btn {
    padding: 0.4rem 0.8rem;
    font-size: 0.85rem;
  }
  
  .flag {
    font-size: 1rem;
  }
}
</style>
