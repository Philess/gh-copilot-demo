import { test, expect } from '@playwright/test';

test.describe('Album Cart Functionality', () => {
  test('Add album to cart and verify cart contents', async ({ page }) => {
    // Step 1: Open the Album App
    await page.goto('http://localhost:3001');
    
    // Wait for albums to load
    await expect(page.locator('h3:has-text("You, Me and an App Id")')).toBeVisible({ timeout: 5000 });
    
    // Step 2: Click "Add to Cart" on the first album
    const firstAlbumAddButton = page.locator('button:has-text("Add to Cart")').first();
    await expect(firstAlbumAddButton).toBeVisible();
    await firstAlbumAddButton.click();
    
    // Verify button changed to "✓ Added"
    await expect(firstAlbumAddButton).toContainText('✓ Added');
    
    // Verify cart badge shows "1"
    const cartBadge = page.locator('button:has-text("🛒") >> text=1');
    await expect(cartBadge).toBeVisible();
    
    // Step 3: Click the cart button on the top right
    const cartButton = page.locator('button:has-text("🛒")');
    await cartButton.click();
    
    // Verify cart sidebar is visible
    const cartHeading = page.locator('h2:has-text("Shopping Cart")');
    await expect(cartHeading).toBeVisible();
    
    // Step 4: Check cart contains the added album
    const cartAlbumTitle = page.locator('text=You, Me and an App Id').nth(1); // Second occurrence is in the cart
    await expect(cartAlbumTitle).toBeVisible();
    
    const cartAlbumArtist = page.locator('text=Daprize').nth(1); // In the cart
    await expect(cartAlbumArtist).toBeVisible();
    
    const cartAlbumPrice = page.locator('text=$10.99').nth(1); // In the cart
    await expect(cartAlbumPrice).toBeVisible();
    
    const cartQuantity = page.locator('text=1').nth(2); // Quantity in the cart
    await expect(cartQuantity).toBeVisible();
    
    const cartTotal = page.locator('text=$10.99').nth(2); // Total in the cart (after price)
    await expect(cartTotal).toBeVisible();
    
    // Step 5: Take a screenshot of the cart
    await page.screenshot({ path: 'cart-screenshot.png', fullPage: false });
    
    // Additional verifications
    expect(page.url()).toContain('localhost:3001');
  });

  test('Add multiple albums to cart', async ({ page }) => {
    // Open app
    await page.goto('http://localhost:3001');
    await expect(page.locator('h3:has-text("You, Me and an App Id")')).toBeVisible({ timeout: 5000 });
    
    // Add first album
    const addButtons = page.locator('button:has-text("Add to Cart")');
    await addButtons.nth(0).click();
    
    // Add second album
    await addButtons.nth(1).click();
    
    // Verify cart badge shows "2"
    const cartBadge = page.locator('button:has-text("🛒")');
    const badgeText = await cartBadge.textContent();
    expect(badgeText).toContain('2');
    
    // Open cart
    await cartBadge.click();
    
    // Verify both albums are in cart
    await expect(page.locator('h2:has-text("Shopping Cart")')).toBeVisible();
    
    // Verify total is correct ($10.99 + $13.99 = $24.98)
    const totalText = page.locator('text=Total:').locator('..').locator('text=$');
    await expect(totalText).toContainText('24.98');
  });

  test('Remove album from cart', async ({ page }) => {
    // Setup: Open app and add album
    await page.goto('http://localhost:3001');
    await expect(page.locator('h3:has-text("You, Me and an App Id")')).toBeVisible({ timeout: 5000 });
    
    const firstAlbumAddButton = page.locator('button:has-text("Add to Cart")').first();
    await firstAlbumAddButton.click();
    
    // Click cart button
    const cartButton = page.locator('button:has-text("🛒")');
    await cartButton.click();
    
    // Verify album is in cart
    await expect(page.locator('text=You, Me and an App Id').nth(1)).toBeVisible();
    
    // Click remove button
    const removeButton = page.locator('button:has-text("🗑️")');
    await removeButton.click();
    
    // Verify album is removed from cart
    await expect(page.locator('text=Your cart is empty')).toBeVisible();
    
    // Verify cart badge is gone or shows 0
    const cartBadge = page.locator('button:has-text("🛒") >> text=1');
    await expect(cartBadge).not.toBeVisible();
  });

  test('Increase and decrease cart item quantity', async ({ page }) => {
    // Setup: Open app and add album
    await page.goto('http://localhost:3001');
    await expect(page.locator('h3:has-text("You, Me and an App Id")')).toBeVisible({ timeout: 5000 });
    
    const firstAlbumAddButton = page.locator('button:has-text("Add to Cart")').first();
    await firstAlbumAddButton.click();
    
    // Click cart button
    const cartButton = page.locator('button:has-text("🛒")');
    await cartButton.click();
    
    // Verify initial quantity is 1
    const quantityDisplay = page.locator('text=1').nth(2); // Quantity in cart
    await expect(quantityDisplay).toContainText('1');
    
    // Click increase button
    const increaseButton = page.locator('button:has-text("+")');
    await increaseButton.click();
    
    // Verify quantity increased to 2
    await expect(page.locator('text=2')).toBeVisible();
    
    // Verify total updated to $21.98 ($10.99 * 2)
    const totalText = page.locator('text=Total:').locator('..').locator('text=$');
    await expect(totalText).toContainText('21.98');
    
    // Click decrease button
    const decreaseButton = page.locator('button:has-text("-")');
    await decreaseButton.click();
    
    // Verify quantity back to 1
    const quantityAfterDecrease = page.locator('text=1').nth(2);
    await expect(quantityAfterDecrease).toBeVisible();
  });
});
