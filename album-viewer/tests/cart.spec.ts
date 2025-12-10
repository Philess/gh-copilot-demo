import { test, expect } from '@playwright/test';

test.describe('Cart Management', () => {
  test('should add album to cart and display it', async ({ page }) => {
    // Step 1: Open the Album App
    await page.goto('http://localhost:3002');
    await page.waitForLoadState('networkidle');
    
    // Wait for albums to load
    await page.waitForSelector('.album-card', { timeout: 10000 });
    console.log('✓ Step 1: Album app loaded successfully');

    // Step 2: Click on "Add to cart" on the first tile
    const firstAlbum = page.locator('.album-card').first();
    const albumTitle = await firstAlbum.locator('.album-title').textContent();
    console.log(`First album title: ${albumTitle}`);
    
    const addToCartButton = firstAlbum.locator('button.btn-primary');
    await expect(addToCartButton).toBeVisible();
    await addToCartButton.click();
    console.log('✓ Step 2: Clicked "Add to Cart" on first album');

    // Verify cart badge shows 1 item
    const cartBadge = page.locator('.cart-icon .badge');
    await expect(cartBadge).toHaveText('1');
    console.log('✓ Cart badge shows 1 item');

    // Step 3: Click on the cart button on the top right to display the cart
    const cartIcon = page.locator('.cart-icon');
    await expect(cartIcon).toBeVisible();
    await cartIcon.click();
    console.log('✓ Step 3: Clicked cart icon');

    // Wait for drawer to open
    const cartDrawer = page.locator('.drawer');
    await expect(cartDrawer).toBeVisible();
    console.log('✓ Cart drawer is visible');

    // Step 4: Check that the cart contains the added album
    const cartItems = page.locator('.cart-item');
    await expect(cartItems).toHaveCount(1);
    
    const cartItemTitle = await cartItems.first().locator('.item-info h3').textContent();
    expect(cartItemTitle).toBe(albumTitle);
    console.log(`✓ Step 4: Cart contains the album: ${cartItemTitle}`);

    // Step 5: Take a screenshot of the cart
    await page.screenshot({ 
      path: 'tests/screenshots/cart-with-item.png',
      fullPage: true 
    });
    console.log('✓ Step 5: Screenshot saved to tests/screenshots/cart-with-item.png');
  });
});
