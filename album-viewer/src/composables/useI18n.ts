import { ref, computed } from 'vue'
import en from '../locales/en'
import fr from '../locales/fr'
import de from '../locales/de'

export type Locale = 'en' | 'fr' | 'de'

const translations = { en, fr, de }

export const localeLabels: Record<Locale, string> = {
  en: 'English',
  fr: 'Français',
  de: 'Deutsch',
}

const currentLocale = ref<Locale>('en')

export function useI18n() {
  const t = computed(() => translations[currentLocale.value])

  function setLocale(locale: Locale) {
    currentLocale.value = locale
  }

  return { t, currentLocale, setLocale, localeLabels }
}
