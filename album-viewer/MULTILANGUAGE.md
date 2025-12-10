# Multi-language Support - Quick Guide

## Overview

The Album Viewer application now supports three languages:
- 🇬🇧 English (Default)
- 🇫🇷 French (Français)
- 🇩🇪 German (Deutsch)

## For Users

### Switching Languages

1. Look for the language selector in the header (top-right corner)
2. Click the dropdown menu
3. Select your preferred language
4. The interface will update immediately
5. Your choice is automatically saved and will be remembered on your next visit

### What Gets Translated

- Application title and subtitle
- Loading messages
- Error messages
- Button labels (Add to Cart, Preview, Try Again)
- Language selector labels

## For Developers

### File Structure

```
src/
├── locales/
│   ├── en.json    # English translations
│   ├── fr.json    # French translations
│   └── de.json    # German translations
├── i18n.ts        # i18n configuration
└── components/
    └── LanguageSelector.vue  # Language switching UI
```

### Configuration

The i18n setup in `src/i18n.ts`:
```typescript
import { createI18n } from 'vue-i18n';
import en from './locales/en.json';
import fr from './locales/fr.json';
import de from './locales/de.json';

const i18n = createI18n({
  legacy: false,           // Use Composition API
  locale: 'en',            // Default language
  fallbackLocale: 'en',    // Fallback if translation missing
  messages: { en, fr, de }
});
```

### Using Translations in Components

```vue
<script setup lang="ts">
import { useI18n } from 'vue-i18n'

const { t } = useI18n()
</script>

<template>
  <h1>{{ t('app.title') }}</h1>
  <p>{{ t('app.subtitle') }}</p>
</template>
```

### Translation File Format

Each locale file follows this structure:

```json
{
  "app": {
    "title": "Album Collection",
    "subtitle": "Discover amazing music albums"
  },
  "loading": {
    "message": "Loading albums..."
  },
  "error": {
    "message": "Failed to load albums...",
    "retry": "Try Again"
  },
  "album": {
    "addToCart": "Add to Cart",
    "preview": "Preview"
  },
  "language": {
    "select": "Language",
    "en": "English",
    "fr": "French",
    "de": "German"
  }
}
```

### Adding New Languages

1. **Create translation file**: `src/locales/es.json` (for Spanish example)
2. **Import in i18n.ts**:
   ```typescript
   import es from './locales/es.json';
   ```
3. **Add to messages**:
   ```typescript
   messages: { en, fr, de, es }
   ```
4. **Update LanguageSelector.vue**:
   ```vue
   <option value="es">{{ t('language.es') }}</option>
   ```
5. **Add language name** to all locale files:
   ```json
   "language": {
     "es": "Spanish" // or "Español", "Spanisch" in respective languages
   }
   ```

### LocalStorage

The selected language is automatically saved:
- Key: `preferredLanguage`
- Value: Language code (en, fr, de)
- Restored on application mount

## Testing

Build and test the application:

```bash
# Install dependencies (if not done)
npm install

# Type check
npm run type-check

# Build
npm run build

# Run development server
npm run dev
```

## Best Practices

1. **Always add keys to all locale files** - Even if you don't have the translation yet, add a placeholder
2. **Use descriptive keys** - `app.title` is better than `title1`
3. **Group related translations** - Use nested objects for organization
4. **Keep fallback language complete** - English (en) should always have all keys
5. **Test language switching** - Verify all UI elements update correctly

## Troubleshooting

### Translation not showing
- Check the key exists in all locale files
- Verify the syntax: `{{ t('key.subkey') }}`
- Check browser console for i18n warnings

### Language not persisting
- Check localStorage in browser DevTools
- Verify the LanguageSelector component is properly saving to localStorage

### Build errors
- Run `npm run type-check` to identify TypeScript issues
- Ensure all JSON files are valid (no trailing commas)

## Dependencies

- `vue-i18n: ^9.8.0` - Official Vue internationalization library

## Resources

- [Vue I18n Documentation](https://vue-i18n.intlify.dev/)
- [Vue 3 Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
