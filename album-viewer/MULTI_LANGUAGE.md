# Multi-Language Support Implementation

## Overview
The Album Viewer application now supports three languages:
- 🇬🇧 **English** (default)
- 🇫🇷 **French** (Français)
- 🇩🇪 **German** (Deutsch)

## Features

### Language Selector
- Located in the header of the application
- Displays flag emoji and language code for each option
- Active language is highlighted
- Language preference is saved to localStorage
- Persists across browser sessions

### Translated Content
All user-facing text is translated, including:
- Header title and subtitle
- Loading messages
- Error messages
- Button labels (Add to Cart, Preview, Try Again)
- Language selector label

## Implementation Details

### File Structure
```
album-viewer/src/
├── i18n/
│   ├── index.ts          # i18n composable and locale management
│   ├── en.ts             # English translations
│   ├── fr.ts             # French translations
│   └── de.ts             # German translations
├── components/
│   ├── LanguageSelector.vue  # Language switcher component
│   ├── AlbumCard.vue         # Updated with translations
│   └── ...
└── App.vue               # Updated with translations and selector
```

### Translation System

#### 1. Translation Files (`i18n/*.ts`)
Each language has its own translation file with the same structure:

```typescript
export default {
  header: {
    title: 'Album Collection',
    subtitle: 'Discover amazing music albums'
  },
  loading: {
    message: 'Loading albums...'
  },
  error: {
    message: 'Failed to load albums...',
    retry: 'Try Again'
  },
  album: {
    addToCart: 'Add to Cart',
    preview: 'Preview'
  },
  language: {
    label: 'Language'
  }
}
```

#### 2. i18n Composable (`i18n/index.ts`)
Provides reactive translation system:

```typescript
const { t, locale, setLocale, languages } = useI18n()

// Access translations
t.value.header.title

// Change language
setLocale('fr')

// Get current locale
console.log(locale.value) // 'en'
```

#### 3. Language Selector Component
- Responsive design for mobile and desktop
- Smooth transitions and hover effects
- Visual feedback for active language
- Accessible with keyboard navigation

### Usage in Components

**In App.vue:**
```vue
<script setup lang="ts">
import { useI18n } from './i18n'

const { t } = useI18n()
</script>

<template>
  <h1>{{ t.header.title }}</h1>
</template>
```

**In AlbumCard.vue:**
```vue
<script setup lang="ts">
import { useI18n } from '../i18n'

const { t } = useI18n()
</script>

<template>
  <button>{{ t.album.addToCart }}</button>
</template>
```

## Translation Samples

### English (Default)
- **Title:** "Album Collection"
- **Subtitle:** "Discover amazing music albums"
- **Add to Cart:** "Add to Cart"

### French
- **Title:** "Collection d'Albums"
- **Subtitle:** "Découvrez des albums de musique incroyables"
- **Add to Cart:** "Ajouter au Panier"

### German
- **Title:** "Album-Sammlung"
- **Subtitle:** "Entdecke fantastische Musikalben"
- **Add to Cart:** "In den Warenkorb"

## Persistence

Language selection is automatically saved to browser's `localStorage`:
- Key: `locale`
- Value: `'en'`, `'fr'`, or `'de'`
- Restored on page reload
- Falls back to English if invalid or not set

## Adding New Languages

To add a new language:

1. **Create translation file:** `src/i18n/[code].ts`
   ```typescript
   export default {
     header: { title: '...', subtitle: '...' },
     loading: { message: '...' },
     // ... complete all keys
   }
   ```

2. **Update i18n/index.ts:**
   ```typescript
   import es from './es'
   
   export type Locale = 'en' | 'fr' | 'de' | 'es'
   
   const translations: Record<Locale, Translation> = {
     en, fr, de, es
   }
   
   export const languages = [
     // ... existing languages
     { code: 'es' as Locale, name: 'Español', flag: '🇪🇸' }
   ]
   ```

3. **Test the new language** by clicking the new language button

## Responsive Design

### Desktop
- Language selector appears on the right side of the header
- Buttons displayed horizontally
- Full language names visible

### Mobile
- Language selector stacks vertically below header title
- Compact button layout
- Smaller fonts for better fit

## Browser Compatibility

- Works in all modern browsers (Chrome, Firefox, Safari, Edge)
- Requires localStorage support
- Gracefully falls back to English if localStorage unavailable

## Testing

To test language switching:

1. Open the app at `http://localhost:3001`
2. Look for the language selector in the header
3. Click on different language buttons (🇬🇧 EN, 🇫🇷 FR, 🇩🇪 DE)
4. Observe all text changes instantly
5. Reload the page - selected language persists

## Future Enhancements

Possible improvements:
- Auto-detect browser language on first visit
- Add more languages (Spanish, Italian, Portuguese, etc.)
- Translate album data (titles, artist names)
- Add date/currency formatting per locale
- Support right-to-left (RTL) languages
- Add language-specific error messages from API
