<template>
  <div class="language-selector">
    <label for="language">{{ t('language.select') }}:</label>
    <select id="language" v-model="currentLocale" @change="changeLanguage" class="language-select">
      <option value="en">{{ t('language.en') }}</option>
      <option value="fr">{{ t('language.fr') }}</option>
      <option value="de">{{ t('language.de') }}</option>
    </select>
  </div>
</template>

<script setup lang="ts">
import { ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'

const { locale, t } = useI18n()
const currentLocale = ref(locale.value)

const changeLanguage = () => {
  locale.value = currentLocale.value
  localStorage.setItem('preferredLanguage', currentLocale.value)
}

// Load saved language preference on mount
const savedLanguage = localStorage.getItem('preferredLanguage')
if (savedLanguage && ['en', 'fr', 'de'].includes(savedLanguage)) {
  currentLocale.value = savedLanguage
  locale.value = savedLanguage
}
</script>

<style scoped>
.language-selector {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  color: white;
}

.language-selector label {
  font-size: 0.9rem;
  font-weight: 500;
}

.language-select {
  padding: 0.5rem 1rem;
  border-radius: 8px;
  border: 2px solid rgba(255, 255, 255, 0.3);
  background: rgba(255, 255, 255, 0.1);
  color: white;
  font-size: 0.9rem;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.3s ease;
  backdrop-filter: blur(10px);
}

.language-select:hover {
  border-color: rgba(255, 255, 255, 0.5);
  background: rgba(255, 255, 255, 0.2);
}

.language-select:focus {
  outline: none;
  border-color: white;
  background: rgba(255, 255, 255, 0.2);
}

.language-select option {
  background: #667eea;
  color: white;
}

@media (max-width: 768px) {
  .language-selector {
    flex-direction: column;
    gap: 0.25rem;
  }
  
  .language-selector label {
    font-size: 0.8rem;
  }
  
  .language-select {
    padding: 0.4rem 0.8rem;
    font-size: 0.85rem;
  }
}
</style>
