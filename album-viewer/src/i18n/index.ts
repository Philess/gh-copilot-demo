import { ref, computed } from 'vue'
import en from './locales/en'
import fr from './locales/fr'
import de from './locales/de'
import type { Translations } from './locales/en'

export type Locale = 'en' | 'fr' | 'de'

const locales: Record<Locale, Translations> = { en, fr, de }

// Module-level reactive state – shared across all components.
const currentLocale = ref<Locale>('en')

export function useI18n() {
  const t = computed(() => locales[currentLocale.value])

  function setLocale(locale: Locale): void {
    currentLocale.value = locale
  }

  return { t, currentLocale, setLocale }
}
