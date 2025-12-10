# Album Viewer

A modern Vue.js 3 application built with TypeScript that displays albums from the albums API.

## Features

- 🎵 Display album collection in a beautiful grid layout
- 🛒 **Shopping cart management with localStorage persistence**
- 🌍 **Multi-language support (English, French, German)**
- 🎨 Modern, responsive design with gradient background
- 🖼️ Album cover images with hover effects
- 💰 Price display for each album
- 📱 Mobile-friendly responsive design
- ⚡ Built with Vue 3, TypeScript, and Vite
- 🔧 Full TypeScript support with type safety
- 📝 Modern Composition API with `<script setup>`
- 💾 Cart and language preferences saved in localStorage

## Prerequisites

- Node.js (v16 or higher)
- npm or yarn
- TypeScript knowledge (helpful but not required)
- The albums-api should be running on `http://localhost:3000`

## Getting Started

1. Install dependencies:
   ```bash
   npm install
   ```

2. Start the development server:
   ```bash
   npm run dev
   ```

3. Open your browser and navigate to `http://localhost:3001`

## API Integration

The app fetches album data from the albums API endpoint `/albums`. Make sure the albums-api is running before starting the Vue app.

The API should return albums in the following format:
```json
[
  {
    "id": 1,
    "title": "Album Title",
    "artist": "Artist Name",
    "price": 10.99,
    "image_url": "https://example.com/image.jpg"
  }
]
```

## Scripts

- `npm run dev` - Start development server
- `npm run build` - Build for production (with TypeScript compilation)
- `npm run preview` - Preview production build
- `npm run type-check` - Run TypeScript type checking without building

## Project Structure

```
album-viewer/
├── src/
│   ├── components/
│   │   ├── AlbumCard.vue          # Individual album card component (TypeScript)
│   │   └── LanguageSelector.vue   # Language selector component
│   ├── locales/
│   │   ├── en.json                # English translations
│   │   ├── fr.json                # French translations
│   │   └── de.json                # German translations
│   ├── types/
│   │   └── album.ts               # TypeScript type definitions
│   ├── App.vue                    # Main app component (TypeScript)
│   ├── main.ts                    # App entry point (TypeScript)
│   └── i18n.ts                    # i18n configuration
├── index.html                     # HTML template
├── vite.config.ts                 # Vite configuration (TypeScript)
├── tsconfig.json                  # TypeScript configuration
├── tsconfig.app.json              # App-specific TypeScript config
├── env.d.ts                       # Environment type declarations
└── package.json                   # Dependencies and scripts
```

## Technologies Used

- Vue 3 (Composition API with `<script setup>`)
- TypeScript (Static type checking and better developer experience)
- Vue I18n (Internationalization and multi-language support)
- Vite (Build tool with TypeScript support)
- Axios (HTTP client with TypeScript generics)
- CSS3 (Grid, Flexbox, Animations)

## TypeScript Features

This application leverages TypeScript for enhanced development experience:

- **Type Safety**: All components, functions, and data structures are strongly typed
- **Interface Definitions**: Clear contracts for data structures (Album interface)
- **Better IDE Support**: Enhanced IntelliSense, auto-completion, and error detection
- **Compile-time Error Checking**: Catch errors before runtime
- **Modern Vue 3 Syntax**: Uses `<script setup lang="ts">` for optimal TypeScript integration

## Features in Detail

### Shopping Cart
Full-featured shopping cart system:
- **Cart icon** in header with item count badge
- **Add to cart** from any album card
- **View cart** with slide-in panel (desktop) or full-screen (mobile)
- **Remove items** individually or clear entire cart
- **Total calculation** with price and item count
- **localStorage persistence** - cart survives page reloads
- **Visual feedback** - buttons show "In Cart" state
- **Responsive design** - optimized for all screen sizes

### Album Cards
Each album is displayed in a card with:
- Album cover image
- Title and artist information
- Price display
- Hover effects with play button overlay
- **Functional Add to Cart button** (changes to "In Cart" when added)
- Preview button

### Responsive Design
The app adapts to different screen sizes:
- Desktop: Multi-column grid layout
- Mobile: Single column layout with stacked buttons

### Error Handling
- Loading spinner while fetching data
- Error message with retry button if API is unavailable
- Fallback placeholder image for broken album covers

### Multi-language Support

The application supports three languages:
- **English (en)** - Default language
- **French (fr)** - Français
- **German (de)** - Deutsch

#### How It Works

1. **Language Selector**: Located in the header, allows users to switch between languages
2. **Translation Files**: JSON files in `src/locales/` contain all translations
3. **Persistence**: Selected language is saved in localStorage and restored on next visit
4. **Vue I18n**: Uses the official Vue internationalization library for seamless language switching

#### Adding New Languages

To add a new language:

1. Create a new JSON file in `src/locales/` (e.g., `es.json` for Spanish)
2. Copy the structure from `en.json` and translate the values
3. Import the new language in `src/i18n.ts`:
   ```typescript
   import es from './locales/es.json'
   ```
4. Add it to the messages object:
   ```typescript
   messages: {
     en, fr, de, es
   }
   ```
5. Add the option to `LanguageSelector.vue`:
   ```vue
   <option value="es">{{ t('language.es') }}</option>
   ```
6. Add the language name translation to all locale files

#### Translation Keys

All translatable text uses keys like `t('app.title')`. The structure is:
- `app.*` - Application header and main content
- `loading.*` - Loading states
- `error.*` - Error messages
- `album.*` - Album card actions
- `language.*` - Language selector labels
