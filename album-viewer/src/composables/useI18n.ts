import { ref, computed } from 'vue'
import en from '../locales/en.json'
import fr from '../locales/fr.json'
import de from '../locales/de.json'

type Locale = 'en' | 'fr' | 'de'
type Translations = typeof en

const translations: Record<Locale, Translations> = {
  en,
  fr,
  de
}

const currentLocale = ref<Locale>('en')

export function useI18n() {
  const t = computed(() => translations[currentLocale.value])
  
  const setLocale = (locale: Locale) => {
    currentLocale.value = locale
    localStorage.setItem('locale', locale)
  }
  
  const locale = computed(() => currentLocale.value)
  
  // Initialize from localStorage
  const savedLocale = localStorage.getItem('locale') as Locale
  if (savedLocale && translations[savedLocale]) {
    currentLocale.value = savedLocale
  }
  
  return {
    t,
    locale,
    setLocale
  }
}
