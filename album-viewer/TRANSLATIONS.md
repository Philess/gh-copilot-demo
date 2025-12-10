# Translation Examples

## Side-by-Side Comparison

### Header Section

| Key | English (EN) 🇬🇧 | French (FR) 🇫🇷 | German (DE) 🇩🇪 |
|-----|------------------|------------------|------------------|
| Title | Album Collection | Collection d'Albums | Album-Sammlung |
| Subtitle | Discover amazing music albums | Découvrez des albums de musique incroyables | Entdecke fantastische Musikalben |

### Loading State

| Key | English (EN) 🇬🇧 | French (FR) 🇫🇷 | German (DE) 🇩🇪 |
|-----|------------------|------------------|------------------|
| Message | Loading albums... | Chargement des albums... | Lade Alben... |

### Error State

| Key | English (EN) 🇬🇧 | French (FR) 🇫🇷 | German (DE) 🇩🇪 |
|-----|------------------|------------------|------------------|
| Message | Failed to load albums. Please make sure the API is running. | Échec du chargement des albums. Veuillez vous assurer que l'API est en cours d'exécution. | Fehler beim Laden der Alben. Bitte stellen Sie sicher, dass die API läuft. |
| Retry Button | Try Again | Réessayer | Erneut Versuchen |

### Album Card

| Key | English (EN) 🇬🇧 | French (FR) 🇫🇷 | German (DE) 🇩🇪 |
|-----|------------------|------------------|------------------|
| Add to Cart | Add to Cart | Ajouter au Panier | In den Warenkorb |
| Preview | Preview | Aperçu | Vorschau |

### Language Selector

| Key | English (EN) 🇬🇧 | French (FR) 🇫🇷 | German (DE) 🇩🇪 |
|-----|------------------|------------------|------------------|
| Label | Language | Langue | Sprache |

## Visual Representation

### English View
```
🎵 Album Collection
Discover amazing music albums

[Language: 🇬🇧 EN | 🇫🇷 FR | 🇩🇪 DE]

[Album Cards with "Add to Cart" and "Preview" buttons]
```

### French View
```
🎵 Collection d'Albums
Découvrez des albums de musique incroyables

[Langue: 🇬🇧 EN | 🇫🇷 FR | 🇩🇪 DE]

[Cartes d'album avec les boutons "Ajouter au Panier" et "Aperçu"]
```

### German View
```
🎵 Album-Sammlung
Entdecke fantastische Musikalben

[Sprache: 🇬🇧 EN | 🇫🇷 FR | 🇩🇪 DE]

[Album-Karten mit den Schaltflächen "In den Warenkorb" und "Vorschau"]
```

## Technical Details

### File: `src/i18n/en.ts`
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
    message: 'Failed to load albums. Please make sure the API is running.',
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

### File: `src/i18n/fr.ts`
```typescript
export default {
  header: {
    title: 'Collection d\'Albums',
    subtitle: 'Découvrez des albums de musique incroyables'
  },
  loading: {
    message: 'Chargement des albums...'
  },
  error: {
    message: 'Échec du chargement des albums. Veuillez vous assurer que l\'API est en cours d\'exécution.',
    retry: 'Réessayer'
  },
  album: {
    addToCart: 'Ajouter au Panier',
    preview: 'Aperçu'
  },
  language: {
    label: 'Langue'
  }
}
```

### File: `src/i18n/de.ts`
```typescript
export default {
  header: {
    title: 'Album-Sammlung',
    subtitle: 'Entdecke fantastische Musikalben'
  },
  loading: {
    message: 'Lade Alben...'
  },
  error: {
    message: 'Fehler beim Laden der Alben. Bitte stellen Sie sicher, dass die API läuft.',
    retry: 'Erneut Versuchen'
  },
  album: {
    addToCart: 'In den Warenkorb',
    preview: 'Vorschau'
  },
  language: {
    label: 'Sprache'
  }
}
```

## Usage in Components

### Example: Using translations in App.vue
```vue
<template>
  <h1>{{ t.header.title }}</h1>
  <p>{{ t.header.subtitle }}</p>
  <p v-if="loading">{{ t.loading.message }}</p>
  <button v-if="error" @click="retry">{{ t.error.retry }}</button>
</template>

<script setup lang="ts">
import { useI18n } from './i18n'

const { t } = useI18n()
</script>
```

### Example: Switching languages
```typescript
import { useI18n } from './i18n'

const { setLocale } = useI18n()

// Switch to French
setLocale('fr')

// Switch to German
setLocale('de')

// Switch to English
setLocale('en')
```

## Testing Checklist

- [x] English translations display correctly
- [x] French translations display correctly
- [x] German translations display correctly
- [x] Language selector shows all options
- [x] Active language is highlighted
- [x] Clicking language button switches instantly
- [x] Selected language persists after page reload
- [x] All components use translations
- [x] No hardcoded strings in components
- [x] Responsive design works on mobile
- [x] TypeScript types are correct
- [x] No console errors
