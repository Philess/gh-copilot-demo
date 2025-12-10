# Feature: Cart Management for Album Viewer

## 📋 Description

Add shopping cart functionality to the Album Viewer application, allowing users to manage a collection of albums they're interested in purchasing. Users should be able to add albums to their cart, view cart contents, see the total number of items, and remove albums from the cart.

## 🎯 User Story

**As a** music enthusiast browsing the album collection  
**I want to** add and remove albums to/from a shopping cart  
**So that** I can keep track of albums I'm interested in purchasing and manage my selections before checkout

## ✨ Features

### 1. Cart Icon in Header
- Display a shopping cart icon in the application header
- Show a badge with the number of albums currently in the cart
- Badge should update in real-time when albums are added/removed
- Cart icon should be clickable to toggle cart view

### 2. Add to Cart Functionality
- Each album card should have an "Add to Cart" button (already exists)
- Clicking the button adds the album to the cart
- Provide visual feedback when an album is added (e.g., button state change, toast notification)
- Prevent duplicate albums in cart (show "Already in Cart" or similar message)
- Button should change to "Remove from Cart" or be disabled if album is already in cart

### 3. Cart Sidebar/Modal
- Display cart contents when cart icon is clicked
- Show a list of albums currently in the cart with:
  - Album cover image (thumbnail)
  - Album title
  - Artist name
  - Price
  - Remove button for each item
- Display cart summary:
  - Total number of items
  - Total price (sum of all album prices)
- Empty cart state with appropriate message
- Close button or click-outside to dismiss

### 4. Remove from Cart
- Each cart item should have a remove/delete button
- Clicking removes the album from the cart immediately
- Update cart count and total price in real-time
- Provide visual feedback on removal

### 5. Cart Persistence
- Cart contents should persist across page refreshes (localStorage)
- Cart should survive browser sessions
- Clear cart option (optional, nice to have)

## 🔧 Implementation Details

### File Structure
```
album-viewer/src/
├── components/
│   ├── CartIcon.vue          # Cart icon with badge in header
│   ├── CartSidebar.vue        # Sliding sidebar/modal with cart contents
│   ├── CartItem.vue           # Individual cart item component
│   └── AlbumCard.vue          # Update to integrate with cart
├── composables/
│   └── useCart.ts             # Cart state management composable
├── types/
│   └── cart.ts                # Cart-related TypeScript interfaces
└── App.vue                    # Update header to include CartIcon
```

### Technical Approach

#### 1. Cart State Management (`composables/useCart.ts`)
```typescript
// Composable with reactive cart state
- cartItems: Ref<Album[]>
- cartCount: ComputedRef<number>
- cartTotal: ComputedRef<number>
- addToCart(album: Album): void
- removeFromCart(albumId: number): void
- isInCart(albumId: number): boolean
- clearCart(): void
- loadCartFromStorage(): void
- saveCartToStorage(): void
```

#### 2. Cart Types (`types/cart.ts`)
```typescript
export interface CartItem {
  album: Album
  addedAt: Date
}

export interface CartState {
  items: CartItem[]
  isOpen: boolean
}
```

#### 3. Components to Create/Update

**CartIcon.vue**
- Display cart icon (🛒 or SVG)
- Badge overlay with cart count
- Click handler to toggle cart sidebar
- Animated transitions for count changes

**CartSidebar.vue**
- Sliding sidebar from right side (or modal)
- Header with "Shopping Cart" title and close button
- List of CartItem components
- Footer with summary (total items, total price)
- Empty state when cart is empty
- Smooth slide-in/out animations

**CartItem.vue**
- Compact horizontal layout
- Album thumbnail (50x50 or similar)
- Album info (title, artist, price)
- Remove button with confirmation (optional)
- Hover effects

**AlbumCard.vue Updates**
- Import useCart composable
- Check if album is in cart (isInCart)
- Conditionally render button text/state
- Call addToCart on button click
- Show feedback (disabled state or different style)

**App.vue Updates**
- Add CartIcon to header
- Add CartSidebar component
- Manage cart open/close state

#### 4. Multi-Language Support
Update translation files to include:
```typescript
cart: {
  title: 'Shopping Cart',
  addToCart: 'Add to Cart',
  removeFromCart: 'Remove from Cart',
  alreadyInCart: 'In Cart',
  empty: 'Your cart is empty',
  itemCount: '{count} item(s)',
  total: 'Total',
  remove: 'Remove',
  clear: 'Clear Cart'
}
```

#### 5. Styling Considerations
- Use existing design system colors and styles
- Responsive design for mobile devices
- Smooth animations (slide-in for sidebar, fade for badges)
- Accessibility (ARIA labels, keyboard navigation)
- Visual feedback for actions (hover, active states)

## ✅ Acceptance Criteria

### Must Have
- [ ] Cart icon is visible in the application header at all times
- [ ] Cart icon displays a badge showing the current number of albums in cart
- [ ] Badge updates immediately when albums are added or removed
- [ ] Clicking "Add to Cart" button on an album card adds the album to cart
- [ ] Albums cannot be added to cart more than once (duplicate prevention)
- [ ] Clicking cart icon opens a sidebar/modal showing cart contents
- [ ] Cart sidebar displays all albums with image, title, artist, and price
- [ ] Each cart item has a remove button that works correctly
- [ ] Cart displays total number of items and total price
- [ ] Empty cart shows appropriate "empty cart" message
- [ ] Cart can be closed by clicking close button or outside the sidebar
- [ ] Cart contents persist across page refreshes (localStorage)
- [ ] All cart text is properly translated in EN, FR, and DE
- [ ] Cart functionality works on mobile and desktop

### Should Have
- [ ] Visual feedback when adding an album (toast notification or button animation)
- [ ] Button on album card changes state when album is in cart (disabled or different text)
- [ ] Smooth animations for sidebar open/close
- [ ] Animated count badge updates
- [ ] Hover effects on cart items and buttons
- [ ] Keyboard navigation support (Escape to close, Tab navigation)

### Could Have
- [ ] "Clear Cart" button to remove all items at once
- [ ] Confirmation dialog before removing items
- [ ] Toast notifications for cart actions
- [ ] "View Cart" button in success message after adding
- [ ] Cart icon shake/pulse animation when items added
- [ ] Undo functionality after removing an item
- [ ] Sort cart items (by date added, price, name)

## 🧪 Testing Checklist

- [ ] Add single album to cart
- [ ] Add multiple albums to cart
- [ ] Try to add same album twice (should prevent or show message)
- [ ] Remove album from cart
- [ ] Remove all albums one by one
- [ ] Cart count updates correctly
- [ ] Total price calculates correctly
- [ ] Refresh page - cart persists
- [ ] Close and reopen browser - cart persists
- [ ] Open cart sidebar, verify all data displays
- [ ] Close cart sidebar (close button)
- [ ] Close cart sidebar (click outside)
- [ ] Test on mobile viewport
- [ ] Test on tablet viewport
- [ ] Test on desktop viewport
- [ ] Switch languages - all cart text translates
- [ ] Check accessibility (screen reader, keyboard navigation)

## 📸 Mockup/Wireframe

### Header with Cart Icon
```
╔════════════════════════════════════════════════════════╗
║  🎵 Album Collection        [EN][FR][DE]   🛒 (3)     ║
║  Discover amazing music                                ║
╚════════════════════════════════════════════════════════╝
```

### Cart Sidebar (Open)
```
╔════════════════════════════════════╗
║  Shopping Cart               ✕     ║
╠════════════════════════════════════╣
║  [img] Album Title 1               ║
║        Artist Name 1        $10.99 ║
║        [Remove]                     ║
║  ────────────────────────────────  ║
║  [img] Album Title 2               ║
║        Artist Name 2        $13.99 ║
║        [Remove]                     ║
║  ────────────────────────────────  ║
║  [img] Album Title 3               ║
║        Artist Name 3        $12.99 ║
║        [Remove]                     ║
╠════════════════════════════════════╣
║  3 items                            ║
║  Total: $37.97                      ║
║  [Clear Cart]                       ║
╚════════════════════════════════════╝
```

### Album Card (Not in Cart)
```
┌──────────────────────┐
│   [Album Image]      │
│                      │
│  Album Title         │
│  Artist Name         │
│  $10.99              │
│                      │
│  [Add to Cart]       │
│  [Preview]           │
└──────────────────────┘
```

### Album Card (In Cart)
```
┌──────────────────────┐
│   [Album Image]      │
│                      │
│  Album Title         │
│  Artist Name         │
│  $10.99              │
│                      │
│  [✓ In Cart]         │
│  [Preview]           │
└──────────────────────┘
```

## 🔗 Related Issues/PRs

- Related to: Album Viewer multi-language support
- Depends on: Album API v2 (for album data)

## 💡 Additional Notes

### Priority
**Medium-High** - Core e-commerce functionality that enhances user experience

### Estimated Effort
**8-12 hours** (depending on developer familiarity with Vue 3 Composition API)

### Breaking Changes
None - This is a new feature addition

### Dependencies
- No new npm packages required
- Uses existing Vue 3 Composition API
- Uses browser localStorage

### Future Enhancements
This feature lays the groundwork for:
- Checkout process
- User authentication and saved carts
- Cart synchronization across devices
- Quantity management (multiple copies of same album)
- Wishlist/favorites functionality
- Cart sharing (share cart via link)

## 📚 Resources

- [Vue 3 Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
- [localStorage API](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage)
- [Vue 3 Transitions](https://vuejs.org/guide/built-ins/transition.html)
- [Accessibility Guidelines](https://www.w3.org/WAI/WCAG21/quickref/)

---

## 📝 Implementation Checklist

- [ ] Create `types/cart.ts` with interfaces
- [ ] Create `composables/useCart.ts` with cart logic
- [ ] Update translation files (en.ts, fr.ts, de.ts)
- [ ] Create `CartIcon.vue` component
- [ ] Create `CartSidebar.vue` component
- [ ] Create `CartItem.vue` component
- [ ] Update `AlbumCard.vue` to integrate cart
- [ ] Update `App.vue` to include cart components
- [ ] Add cart styling and animations
- [ ] Test all functionality
- [ ] Update README with cart feature documentation
- [ ] Create PR with all changes

---

**Labels:** `enhancement`, `feature`, `frontend`, `vue`, `good-first-issue`  
**Milestone:** Q1 2026  
**Assignee:** TBD
