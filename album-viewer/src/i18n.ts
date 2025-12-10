import { createI18n } from 'vue-i18n';
import en from './locales/en.json';
import fr from './locales/fr.json';
import de from './locales/de.json';

export const SUPPORTED_LOCALES = ['en', 'fr', 'de'] as const;
export type Locale = typeof SUPPORTED_LOCALES[number];

const localeFromStorage = localStorage.getItem('locale') as Locale | null;
const defaultLocale: Locale = localeFromStorage && SUPPORTED_LOCALES.includes(localeFromStorage)
  ? localeFromStorage
  : 'en';

export const i18n = createI18n({
  legacy: false,
  locale: defaultLocale,
  fallbackLocale: 'en',
  messages: {
    en,
    fr,
    de,
  },
});

export function setLocale(locale: Locale) {
  if (!SUPPORTED_LOCALES.includes(locale)) return;
  i18n.global.locale.value = locale;
  localStorage.setItem('locale', locale);
}
