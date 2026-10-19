import { expect, test } from '@playwright/test'

test('adds the first album to the cart and captures its contents', async ({ page }, testInfo) => {
  const firstAlbum = page.locator('.album-card').first()
  const cart = page.getByRole('region', { name: 'Your cart', exact: true })
  let title = ''
  let artist = ''
  let price = ''

  await test.step('1. Open Album App and verify the first tile is ready', async () => {
    const response = await page.goto('/')
    expect(response?.ok()).toBe(true)
    await expect(firstAlbum).toBeVisible()
    await expect(firstAlbum.getByRole('button', { name: 'Add to Cart', exact: true })).toBeEnabled()
    await expect(page.getByRole('button', { name: 'Cart, 0 albums', exact: true })).toBeVisible()
    title = await firstAlbum.locator('.album-title').innerText()
    artist = await firstAlbum.locator('.album-artist').innerText()
    price = await firstAlbum.locator('.price').innerText()
    expect(title.trim()).not.toBe('')
    expect(artist.trim()).not.toBe('')
    expect(price).toMatch(/^\$\d+\.\d{2}$/)
  })

  await test.step('2. Add the first album and verify the selection', async () => {
    await firstAlbum.getByRole('button', { name: 'Add to Cart', exact: true }).click()
    await expect(firstAlbum.getByRole('button', { name: 'In Cart', exact: true })).toBeDisabled()
    await expect(page.locator('.cart-count')).toHaveText('1')
  })

  await test.step('3. Open the cart and verify it is displayed', async () => {
    const trigger = page.getByRole('button', { name: 'Cart, 1 albums', exact: true })
    await trigger.click()
    await expect(trigger).toHaveAttribute('aria-expanded', 'true')
    await expect(cart).toBeVisible()
  })

  await test.step('4. Verify the cart contains exactly the album added', async () => {
    const item = cart.getByRole('listitem')
    await expect(item).toHaveCount(1)
    await expect(item.getByRole('heading', { name: title, exact: true })).toBeVisible()
    await expect(item.getByText(artist, { exact: true })).toBeVisible()
    await expect(item.getByText(price, { exact: true })).toBeVisible()
    await expect(item.getByRole('button', {
      name: `Remove ${title} by ${artist} from cart`, exact: true
    })).toBeVisible()
  })

  await test.step('5. Capture the verified cart', async () => {
    const screenshot = testInfo.outputPath('album-cart.png')
    await cart.screenshot({ path: screenshot })
    await testInfo.attach('Verified cart', { path: screenshot, contentType: 'image/png' })
  })
})
