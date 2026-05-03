import { test, expect } from '@playwright/test';

test.describe('Album App Cart Functionality', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the app
    await page.goto('http://localhost:3002');
    
    // Wait for albums to load
    await page.waitForSelector('.album-card', { timeout: 10000 });
  });

  test('should add album to cart and display it correctly', async ({ page }) => {
    // Step 1: Open the Album App (done in beforeEach)
    await expect(page).toHaveTitle('Album Viewer');
    
    // Verify the page has loaded with albums
    const albumCards = page.locator('.album-card');
    await expect(albumCards).toHaveCount(6); // Assuming 6 albums
    
    // Step 2: Click on "Add to cart" on the first tile
    const firstAlbumCard = albumCards.first();
    const albumTitle = await firstAlbumCard.locator('h3').textContent();
    const addToCartButton = firstAlbumCard.locator('button.btn-primary');
    
    console.log(`Adding album to cart: ${albumTitle}`);
    await addToCartButton.click();
    
    // Verify the cart badge appears or updates
    const cartButton = page.locator('.cart-button');
    const cartBadge = cartButton.locator('.cart-badge');
    await expect(cartBadge).toBeVisible();
    await expect(cartBadge).toHaveText('1');
    
    // Step 3: Click on the cart button on the top right to display the cart
    await cartButton.click();
    
    // Wait for cart modal to appear
    const cartModal = page.locator('.modal-overlay');
    await expect(cartModal).toBeVisible();
    
    // Verify cart modal header
    const cartHeader = page.locator('.modal-header h2');
    await expect(cartHeader).toContainText('Shopping Cart');
    
    // Step 4: Check that the cart contains the added album
    const cartItems = page.locator('.cart-item');
    await expect(cartItems).toHaveCount(1);
    
    // Verify the cart item details
    const cartItemTitle = cartItems.first().locator('h3');
    await expect(cartItemTitle).toHaveText(albumTitle || '');
    
    // Verify quantity
    const quantity = cartItems.first().locator('.quantity');
    await expect(quantity).toHaveText('1');
    
    // Verify total
    const totalAmount = page.locator('.total-amount');
    await expect(totalAmount).toBeVisible();
    await expect(totalAmount).toContainText('$');
    
    // Step 5: Take a screenshot of the cart
    await page.screenshot({ 
      path: 'tests/screenshots/cart-with-album.png',
      fullPage: true 
    });
    
    console.log('✅ Test completed successfully!');
    console.log(`   - Album added: ${albumTitle}`);
    console.log(`   - Cart displays correctly`);
    console.log(`   - Screenshot saved: tests/screenshots/cart-with-album.png`);
  });

  test('should update cart quantity when adding same album multiple times', async ({ page }) => {
    const firstAlbumCard = page.locator('.album-card').first();
    const addToCartButton = firstAlbumCard.locator('button.btn-primary');
    
    // Add album twice
    await addToCartButton.click();
    await addToCartButton.click();
    
    // Check cart badge
    const cartBadge = page.locator('.cart-badge');
    await expect(cartBadge).toHaveText('2');
    
    // Open cart
    await page.locator('.cart-button').click();
    
    // Verify quantity in cart
    const quantity = page.locator('.cart-item .quantity');
    await expect(quantity).toHaveText('2');
  });

  test('should increase and decrease item quantity in cart', async ({ page }) => {
    // Add album to cart
    await page.locator('.album-card').first().locator('button.btn-primary').click();
    
    // Open cart
    await page.locator('.cart-button').click();
    
    // Get initial quantity
    const quantity = page.locator('.cart-item .quantity');
    await expect(quantity).toHaveText('1');
    
    // Click increase button
    const increaseButton = page.locator('.cart-item .qty-btn:has-text("+")');
    await increaseButton.click();
    await expect(quantity).toHaveText('2');
    
    // Click decrease button
    const decreaseButton = page.locator('.cart-item .qty-btn:has-text("-")');
    await decreaseButton.click();
    await expect(quantity).toHaveText('1');
  });

  test('should remove item from cart', async ({ page }) => {
    // Add album to cart
    await page.locator('.album-card').first().locator('button.btn-primary').click();
    
    // Open cart
    await page.locator('.cart-button').click();
    
    // Verify item is in cart
    await expect(page.locator('.cart-item')).toHaveCount(1);
    
    // Click remove button
    await page.locator('.remove-btn').click();
    
    // Verify cart is empty
    await expect(page.locator('.empty-cart')).toBeVisible();
    await expect(page.locator('.empty-cart p')).toHaveText('Your cart is empty');
  });

  test('should clear all items from cart', async ({ page }) => {
    // Add multiple albums to cart
    const albumCards = page.locator('.album-card');
    await albumCards.nth(0).locator('button.btn-primary').click();
    await albumCards.nth(1).locator('button.btn-primary').click();
    
    // Open cart
    await page.locator('.cart-button').click();
    
    // Verify multiple items
    await expect(page.locator('.cart-item')).toHaveCount(2);
    
    // Click clear cart
    await page.locator('button:has-text("Clear Cart")').click();
    
    // Verify cart is empty
    await expect(page.locator('.empty-cart')).toBeVisible();
  });

  test('should close cart modal when clicking close button', async ({ page }) => {
    // Add album and open cart
    await page.locator('.album-card').first().locator('button.btn-primary').click();
    await page.locator('.cart-button').click();
    
    // Verify cart is open
    await expect(page.locator('.modal-overlay')).toBeVisible();
    
    // Click close button
    await page.locator('.close-btn').click();
    
    // Verify cart is closed
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
  });

  test('should close cart modal when clicking overlay', async ({ page }) => {
    // Add album and open cart
    await page.locator('.album-card').first().locator('button.btn-primary').click();
    await page.locator('.cart-button').click();
    
    // Verify cart is open
    const modalOverlay = page.locator('.modal-overlay');
    await expect(modalOverlay).toBeVisible();
    
    // Click overlay (outside modal content)
    await modalOverlay.click({ position: { x: 10, y: 10 } });
    
    // Verify cart is closed
    await expect(modalOverlay).not.toBeVisible();
  });
});
