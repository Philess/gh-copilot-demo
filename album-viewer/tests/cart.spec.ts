import { test, expect } from '@playwright/test';

test.describe('Album Viewer - Shopping Cart', () => {
  test('should add album to cart and verify in cart panel', async ({ page }) => {
    // Step 1: Navigate to the Album App
    console.log('Step 1: Opening http://localhost:3001');
    await page.goto('http://localhost:3001');
    
    // Wait for the page to load
    await page.waitForLoadState('networkidle');
    
    // Verify the page loaded successfully
    await expect(page.locator('h1')).toContainText('Album Collection');
    console.log('✓ Page loaded successfully');

    // Step 2: Click on "Add to cart" on the first album tile
    console.log('\nStep 2: Clicking "Add to cart" on the first album');
    
    // Wait for albums to be loaded
    await page.waitForSelector('.album-card', { timeout: 10000 });
    
    // Get the first album card
    const firstAlbumCard = page.locator('.album-card').first();
    await expect(firstAlbumCard).toBeVisible();
    
    // Get the album title before adding to cart
    const albumTitle = await firstAlbumCard.locator('.album-title').textContent();
    const albumArtist = await firstAlbumCard.locator('.album-artist').textContent();
    const albumPrice = await firstAlbumCard.locator('.price').textContent();
    
    console.log(`  Album: ${albumTitle}`);
    console.log(`  Artist: ${albumArtist}`);
    console.log(`  Price: ${albumPrice}`);
    
    // Click the "Add to Cart" button
    const addToCartButton = firstAlbumCard.locator('.btn-primary');
    await expect(addToCartButton).toBeVisible();
    await addToCartButton.click();
    
    // Verify button changed to "In Cart"
    await expect(addToCartButton).toContainText('In Cart');
    console.log('✓ Album added to cart successfully');

    // Step 3: Click on the cart button on the top right
    console.log('\nStep 3: Opening cart panel');
    
    // Wait for cart badge to appear with count "1"
    const cartBadge = page.locator('.cart-badge');
    await expect(cartBadge).toBeVisible();
    await expect(cartBadge).toContainText('1');
    console.log('✓ Cart badge shows 1 item');
    
    // Click on the cart icon to open the cart panel
    const cartIcon = page.locator('.cart-icon');
    await cartIcon.click();
    
    // Wait for cart panel to be visible
    await page.waitForSelector('.cart-panel', { state: 'visible', timeout: 5000 });
    console.log('✓ Cart panel opened');

    // Step 4: Check that the cart contains the added album
    console.log('\nStep 4: Verifying cart contents');
    
    // Verify cart header
    await expect(page.locator('.cart-header h2')).toBeVisible();
    
    // Verify cart item exists
    const cartItem = page.locator('.cart-item').first();
    await expect(cartItem).toBeVisible();
    
    // Verify the album details in cart
    const cartItemTitle = await cartItem.locator('.item-title').textContent();
    const cartItemArtist = await cartItem.locator('.item-artist').textContent();
    const cartItemPrice = await cartItem.locator('.item-price').textContent();
    
    console.log(`  Cart contains:`);
    console.log(`    Title: ${cartItemTitle}`);
    console.log(`    Artist: ${cartItemArtist}`);
    console.log(`    Price: ${cartItemPrice}`);
    
    // Verify it's the same album we added
    expect(cartItemTitle).toBe(albumTitle);
    expect(cartItemArtist).toBe(albumArtist);
    expect(cartItemPrice).toBe(albumPrice);
    console.log('✓ Cart contains the correct album');
    
    // Verify cart summary
    const totalItems = await page.locator('.cart-summary .summary-value').first().textContent();
    expect(totalItems).toContain('1');
    console.log(`✓ Cart summary shows: ${totalItems}`);

    // Step 5: Take a screenshot of the cart
    console.log('\nStep 5: Taking screenshot');
    await page.screenshot({ 
      path: 'test-results/cart-with-album.png',
      fullPage: true 
    });
    console.log('✓ Screenshot saved to test-results/cart-with-album.png');
    
    console.log('\n✅ All test steps completed successfully!');
  });
  
  test('should allow removing item from cart', async ({ page }) => {
    // Navigate and add item
    await page.goto('http://localhost:3001');
    await page.waitForLoadState('networkidle');
    
    // Add first album to cart
    await page.waitForSelector('.album-card');
    const firstAlbumCard = page.locator('.album-card').first();
    await firstAlbumCard.locator('.btn-primary').click();
    
    // Open cart
    await page.locator('.cart-icon').click();
    await page.waitForSelector('.cart-panel', { state: 'visible' });
    
    // Remove item
    const removeButton = page.locator('.remove-btn').first();
    await removeButton.click();
    
    // Verify empty state
    await expect(page.locator('.empty-cart')).toBeVisible();
    await expect(page.locator('.empty-title')).toContainText('Your cart is empty');
    
    console.log('✓ Item removed successfully, empty state displayed');
  });
  
  test('should persist cart after page reload', async ({ page }) => {
    // Navigate and add item
    await page.goto('http://localhost:3001');
    await page.waitForLoadState('networkidle');
    
    // Add first album to cart
    await page.waitForSelector('.album-card');
    const firstAlbumCard = page.locator('.album-card').first();
    const albumTitle = await firstAlbumCard.locator('.album-title').textContent();
    await firstAlbumCard.locator('.btn-primary').click();
    
    // Reload the page
    await page.reload();
    await page.waitForLoadState('networkidle');
    
    // Verify cart badge still shows 1
    const cartBadge = page.locator('.cart-badge');
    await expect(cartBadge).toBeVisible();
    await expect(cartBadge).toContainText('1');
    
    // Open cart and verify item is still there
    await page.locator('.cart-icon').click();
    await page.waitForSelector('.cart-panel', { state: 'visible' });
    
    const cartItemTitle = await page.locator('.cart-item .item-title').first().textContent();
    expect(cartItemTitle).toBe(albumTitle);
    
    console.log('✓ Cart persisted after page reload');
  });
});
