# Album Viewer

A modern Vue.js 3 application built with TypeScript that displays albums from the albums API.

## Features

- 🎵 Display album collection in a beautiful grid layout
- 🎨 Modern, responsive design with gradient background
- 🖼️ Album cover images with hover effects
- 💰 Price display for each album
- 📱 Mobile-friendly responsive design
- ⚡ Built with Vue 3, TypeScript, and Vite
- 🔧 Full TypeScript support with type safety
- 📝 Modern Composition API with `<script setup>`

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
- `npm test -- --run` - Run unit and cart DOM integration tests once
- `npm run test:e2e` - Run the Playwright cart scenario against `http://localhost:3107`

### Playwright Cart Scenario

With the album API running on `http://localhost:3000`, start the frontend using
`npm run dev -- --host localhost --port 3107 --strictPort`. In another terminal,
from `album-viewer`, run:

```powershell
npx playwright install chromium
npm run test:e2e
```

The test uses real API data. It opens the app, selects the first album, opens
the cart, checks its title, artist, price, and item count, then captures the cart.
Each step must pass before the next step runs. The screenshot is saved under
`../.playwright-mcp/test-results` and attached to the test result.
Files named `*.pw.ts` are Playwright tests, separate from the Vitest tests.

## Project Structure

```
album-viewer/
├── src/
│   ├── components/
│   │   └── AlbumCard.vue    # Individual album card component (TypeScript)
│   ├── types/
│   │   └── album.ts         # TypeScript type definitions
│   ├── App.vue              # Main app component (TypeScript)
│   └── main.ts              # App entry point (TypeScript)
├── index.html               # HTML template
├── vite.config.ts           # Vite configuration (TypeScript)
├── tsconfig.json            # TypeScript configuration
├── tsconfig.app.json        # App-specific TypeScript config
├── env.d.ts                 # Environment type declarations
└── package.json             # Dependencies and scripts
```

## Technologies Used

- Vue 3 (Composition API with `<script setup>`)
- TypeScript (Static type checking and better developer experience)
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

### Album Cards
Each album is displayed in a card with:
- Album cover image
- Title and artist information
- Price display
- Hover effects with play button overlay
- Add to Cart and Preview buttons

### Responsive Design
The app adapts to different screen sizes:
- Desktop: Multi-column grid layout
- Mobile: Single column layout with stacked buttons

### Cart Management
- Use **Add to Cart** on an album card to select it. The header cart icon shows
  the current count, including zero.
- Each album can appear only once, identified by its ID. Selected cards show a
  disabled **In Cart** button.
- Activate the header cart button to open a non-modal panel showing the selected
  albums' titles, artists, and prices. Use **Remove** to delete a selection; the
  count and card buttons update immediately, and removed albums can be added again.
- An empty cart displays **Your cart is empty**.
- Use **Close cart**, the header cart button, or Escape while focus is inside the
  panel to close it. Opening moves focus into the panel; closing returns focus to
  the header button. Removing an item moves focus to the next remove action,
  the previous one if it was last, or the close button when the cart becomes empty.
- The cart stays in memory while the app is open, including when the panel is
  closed. Refreshing the page resets it. There is no checkout, quantity control,
  backend persistence, or browser-storage persistence.

### Error Handling
- Loading spinner while fetching data
- Error message with retry button if API is unavailable
- Fallback placeholder image for broken album covers
