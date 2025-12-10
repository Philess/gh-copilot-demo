<template>
  <div class="language-selector">
    <label for="language-select" class="language-label">
      {{ t.language.select }}:
    </label>
    <select 
      id="language-select"
      v-model="currentLocale" 
      @change="handleLanguageChange"
      class="language-select"
    >
      <option value="en">🇬🇧 {{ t.language.en }}</option>
      <option value="fr">🇫🇷 {{ t.language.fr }}</option>
      <option value="de">🇩🇪 {{ t.language.de }}</option>
    </select>
  </div>
</template>

<script setup lang="ts">
import { computed } from 'vue'
import { useI18n } from '../composables/useI18n'

const { t, locale, setLocale } = useI18n()

const currentLocale = computed({
  get: () => locale.value,
  set: (value) => setLocale(value as 'en' | 'fr' | 'de')
})

const handleLanguageChange = () => {
  console.log(`Language changed to: ${currentLocale.value}`)
}
</script>

<style scoped>
.language-selector {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.language-label {
  color: white;
  font-size: 0.9rem;
  font-weight: 500;
}

.language-select {
  background: rgba(255, 255, 255, 0.2);
  color: white;
  border: 2px solid rgba(255, 255, 255, 0.3);
  padding: 0.5rem 1rem;
  border-radius: 8px;
  font-size: 0.9rem;
  cursor: pointer;
  transition: all 0.3s ease;
  backdrop-filter: blur(10px);
}

.language-select:hover {
  background: rgba(255, 255, 255, 0.3);
  border-color: rgba(255, 255, 255, 0.5);
}

.language-select:focus {
  outline: none;
  border-color: white;
  box-shadow: 0 0 0 3px rgba(255, 255, 255, 0.1);
}

.language-select option {
  background: #667eea;
  color: white;
  padding: 0.5rem;
}

@media (max-width: 768px) {
  .language-selector {
    flex-direction: column;
    align-items: flex-start;
    gap: 0.25rem;
  }
  
  .language-select {
    width: 100%;
  }
}
</style>
