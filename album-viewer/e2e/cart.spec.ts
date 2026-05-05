import { test, expect } from '@playwright/test'
import path from 'path'

const APP_URL = 'http://localhost:3001'

test('add first album to cart and verify cart contents', async ({ page }) => {
  // Step 1: Open the Album App
  await page.goto(APP_URL)
  await expect(page).toHaveTitle('Album Viewer')
  await expect(page.getByRole('heading', { name: /Album Collection/ })).toBeVisible()

  // Wait for albums to load
  const firstCard = page.locator('.album-card').first()
  await expect(firstCard).toBeVisible()

  // Capture the first album title for later assertion
  const firstAlbumTitle = await firstCard.getByRole('heading').textContent()

  // Step 2: Click "Add to Cart" on the first album card
  await firstCard.getByRole('button', { name: 'Add to Cart' }).click()

  // Verify button changes to "In Cart" (disabled) to confirm the item was added
  await expect(firstCard.getByRole('button', { name: 'In Cart' })).toBeDisabled()

  // Verify cart badge shows 1 item
  const cartButton = page.getByRole('button', { name: 'My Cart' })
  await expect(cartButton.locator('.cart-badge')).toHaveText('1')

  // Step 3: Click the cart icon to open the cart drawer
  await cartButton.click()

  // Verify drawer is visible
  const cartDrawer = page.locator('.cart-drawer')
  await expect(cartDrawer).toBeVisible()
  await expect(cartDrawer.getByRole('heading', { name: 'My Cart' })).toBeVisible()

  // Step 4: Verify the cart contains the added album
  const cartItem = cartDrawer.locator('.cart-item').first()
  await expect(cartItem).toBeVisible()
  await expect(cartItem.locator('.cart-item-title')).toHaveText(firstAlbumTitle!)
  await expect(cartItem.locator('.cart-item-artist')).not.toBeEmpty()
  await expect(cartItem.locator('.cart-item-price')).toContainText('$')
  await expect(cartDrawer.locator('.cart-item-count')).toHaveText('1 item(s)')
  await expect(cartDrawer.locator('.cart-total')).toContainText('Total:')

  // Step 5: Take a screenshot of the cart
  await page.screenshot({
    path: path.join('e2e', 'screenshots', 'cart-with-album.png'),
    fullPage: false,
  })
})
