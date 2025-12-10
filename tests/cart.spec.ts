import { test, expect } from '@playwright/test';

test('Add album to cart and verify', async ({ page }) => {
  // Step 1: Open the Album App
  console.log('Step 1: Navigating to http://localhost:3001');
  await page.goto('http://localhost:3001');
  
  // Wait for the page to load and albums to appear
  await page.waitForSelector('.album-card', { timeout: 10000 });
  console.log('✓ Page loaded with albums');

  // Step 2: Click on "Add to cart" on the first tile
  console.log('Step 2: Looking for first "Add to Cart" button');
  const firstAddToCartButton = page.locator('.album-card').first().locator('button:has-text("Add to Cart")');
  await expect(firstAddToCartButton).toBeVisible();
  await firstAddToCartButton.click();
  console.log('✓ Clicked "Add to Cart" on first album');

  // Wait a moment for the cart to update
  await page.waitForTimeout(500);

  // Step 3: Click on the cart button on the top right
  console.log('Step 3: Looking for cart icon in header');
  const cartButton = page.locator('header').locator('button:has-text("🛒"), button:has-text("Cart"), .cart-icon, [aria-label*="cart" i]').first();
  await expect(cartButton).toBeVisible();
  await cartButton.click();
  console.log('✓ Clicked cart button');

  // Wait for cart panel to appear
  await page.waitForTimeout(500);

  // Step 4: Check that the cart contains the added album
  console.log('Step 4: Verifying cart contains the album');
  const cartPanel = page.locator('.cart-panel, .cart-drawer, [role="dialog"]').first();
  await expect(cartPanel).toBeVisible();
  
  // Verify at least one item is in the cart
  const cartItems = cartPanel.locator('.cart-item, .album-card');
  await expect(cartItems).toHaveCount(1, { timeout: 5000 });
  console.log('✓ Cart contains 1 album');

  // Step 5: Take a screenshot of the cart
  console.log('Step 5: Taking screenshot of the cart');
  await page.screenshot({ 
    path: 'cart-screenshot.png',
    fullPage: true 
  });
  console.log('✓ Screenshot saved as cart-screenshot.png');
});
