import { defineConfig } from '@playwright/test'

export default defineConfig({
  testDir: './e2e',
  testMatch: '**/*.pw.ts',
  outputDir: '../.playwright-mcp/test-results',
  use: {
    baseURL: 'http://localhost:3107',
    browserName: 'chromium'
  }
})
