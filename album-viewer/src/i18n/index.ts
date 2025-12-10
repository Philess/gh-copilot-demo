import { ref, computed } from 'vue'
import en from './en'
import fr from './fr'
import de from './de'

export type Locale = 'en' | 'fr' | 'de'

export interface Translation {
  header: {
    title: string
    subtitle: string
  }
  loading: {
    message: string
  }
  error: {
    message: string
    retry: string
  }
  album: {
    addToCart: string
    preview: string
  }
  cart: {
    title: string
    addToCart: string
    removeFromCart: string
    inCart: string
    empty: string
    emptyDescription: string
    itemCount: string
    total: string
    clear: string
    close: string
    addedToCart: string
    alreadyInCart: string
  }
  language: {
    label: string
  }
}

const translations: Record<Locale, Translation> = {
  en,
  fr,
  de
}

export const languages = [
  { code: 'en' as Locale, name: 'English', flag: '🇬🇧' },
  { code: 'fr' as Locale, name: 'Français', flag: '🇫🇷' },
  { code: 'de' as Locale, name: 'Deutsch', flag: '🇩🇪' }
]

// Get locale from localStorage or default to 'en'
const getInitialLocale = (): Locale => {
  const stored = localStorage.getItem('locale')
  if (stored && (stored === 'en' || stored === 'fr' || stored === 'de')) {
    return stored as Locale
  }
  return 'en'
}

const currentLocale = ref<Locale>(getInitialLocale())

export const useI18n = () => {
  const t = computed<Translation>(() => translations[currentLocale.value])
  
  const setLocale = (locale: Locale) => {
    currentLocale.value = locale
    localStorage.setItem('locale', locale)
  }
  
  const locale = computed(() => currentLocale.value)
  
  return {
    t,
    locale,
    setLocale,
    languages
  }
}
