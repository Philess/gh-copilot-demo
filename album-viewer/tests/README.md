# Album Viewer E2E Tests

This directory contains end-to-end tests for the Album Viewer application using Playwright.

## Prerequisites

1. Install Playwright and its dependencies:
   ```bash
   npm install -D @playwright/test
   npm run playwright:install
   ```

2. Make sure the API server is running on port 3000 (or update the proxy in vite.config.ts)

## Running Tests

### Run all tests (headless):
```bash
npm run test:e2e
```

### Run tests with UI mode (recommended for development):
```bash
npm run test:e2e:ui
```

### Run tests in headed mode (see the browser):
```bash
npm run test:e2e:headed
```

### Run a specific test file:
```bash
npx playwright test tests/cart.spec.ts
```

### Run tests in debug mode:
```bash
npx playwright test --debug
```

## Test Scenarios

### cart.spec.ts
Tests the shopping cart functionality:

1. ✅ Add album to cart and display it correctly
   - Opens the app
   - Clicks "Add to Cart" on the first album
   - Clicks the cart button in the top right
   - Verifies the cart contains the added album
   - Takes a screenshot

2. ✅ Update cart quantity when adding same album multiple times
3. ✅ Increase and decrease item quantity in cart
4. ✅ Remove item from cart
5. ✅ Clear all items from cart
6. ✅ Close cart modal when clicking close button
7. ✅ Close cart modal when clicking overlay

## Test Results

- HTML reports are generated in `playwright-report/`
- Screenshots are saved in `tests/screenshots/`
- Test artifacts are stored in `test-results/`

## Viewing Test Reports

After running tests, view the HTML report:
```bash
npx playwright show-report
```

## Configuration

The Playwright configuration is defined in `playwright.config.ts`. Key settings:

- Base URL: `http://localhost:3002`
- Browser: Chromium (Desktop Chrome)
- Automatic dev server startup
- Screenshot on failure
- Trace on first retry

## Troubleshooting

If tests fail:

1. Ensure the dev server is running: `npm run dev`
2. Check that albums are loading correctly in the browser
3. Verify the API is responding at the configured port
4. Run tests in headed mode to see what's happening: `npm run test:e2e:headed`
5. Use debug mode for step-by-step execution: `npx playwright test --debug`

## Writing New Tests

Follow this pattern:

```typescript
import { test, expect } from '@playwright/test';

test.describe('Feature Name', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    // Setup code
  });

  test('should do something', async ({ page }) => {
    // Test code with assertions
    await expect(page.locator('.element')).toBeVisible();
  });
});
```

For more information, see the [Playwright documentation](https://playwright.dev/).
