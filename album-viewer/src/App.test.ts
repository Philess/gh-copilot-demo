// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick, type App as VueApp } from 'vue'
import axios from 'axios'
import App from './App.vue'
import AlbumCard from './components/AlbumCard.vue'
import type { Album } from './types/album'

vi.mock('axios', () => ({ default: { get: vi.fn() } }))

const albums: Album[] = [
  { id: 1, title: 'Album A', artist: 'Artist A', price: 10.5, image_url: '/a.jpg' },
  { id: 2, title: 'Album B', artist: 'Artist B', price: 20, image_url: '/b.jpg' },
  { id: 3, title: 'Album C', artist: 'Artist C', price: 9.99, image_url: '/c.jpg' }
]

let app: VueApp | undefined
let host: HTMLDivElement

const settle = async (): Promise<void> => {
  await Promise.resolve()
  await nextTick()
}

const mount = async (): Promise<void> => {
  host = document.createElement('div')
  document.body.append(host)
  app = createApp(App)
  app.mount(host)
  await settle()
}

const button = (selector: string): HTMLButtonElement => {
  const element = host.querySelector(selector)
  if (!(element instanceof HTMLButtonElement)) {
    throw new Error(`Button not found: ${selector}`)
  }
  return element
}

const addButton = (index: number): HTMLButtonElement =>
  button(`.album-card:nth-child(${index + 1}) .btn-primary`)

const click = async (element: HTMLElement): Promise<void> => {
  element.click()
  await settle()
}

const expectCount = (count: number): void => {
  expect(host.querySelector('.cart-count')?.textContent).toBe(String(count))
  expect(button('.cart-trigger').getAttribute('aria-label')).toBe(`Cart, ${count} albums`)
  expect(host.querySelector('[role="status"]')?.textContent?.trim()).toBe(`${count} albums in cart`)
}

beforeEach(() => {
  vi.mocked(axios.get).mockReset()
  vi.mocked(axios.get).mockResolvedValue({ data: albums })
})

afterEach(() => {
  app?.unmount()
  app = undefined
  document.body.replaceChildren()
  vi.restoreAllMocks()
})

describe('cart management', () => {
  it('starts empty, opens a labelled non-modal panel, and restores focus on close', async () => {
    await mount()
    expect(axios.get).toHaveBeenCalledWith('/albums')
    expectCount(0)
    expect(host.querySelector('#cart-panel')).toBeNull()
    await click(button('.cart-trigger'))
    const panel = host.querySelector('#cart-panel')
    expect(document.activeElement).toBe(panel)
    expect(panel?.getAttribute('aria-labelledby')).toBe('cart-title')
    expect(panel?.getAttribute('aria-modal')).toBeNull()
    expect(panel?.textContent).toContain('Your cart is empty')
    expect(button('.cart-trigger').getAttribute('aria-expanded')).toBe('true')
    await click(button('.cart-heading button'))
    expect(host.querySelector('#cart-panel')).toBeNull()
    expect(document.activeElement).toBe(button('.cart-trigger'))
    expect(button('.cart-trigger').getAttribute('aria-expanded')).toBe('false')
  })

  it('adds unique albums, prevents rapid duplicates, and shows exact item details', async () => {
    await mount()
    const first = addButton(0)
    first.click()
    first.click()
    first.click()
    await settle()
    expectCount(1)
    expect(first.disabled).toBe(true)
    expect(first.textContent?.trim()).toBe('In Cart')
    await click(addButton(1))
    expectCount(2)
    await click(button('.cart-trigger'))
    const items = host.querySelectorAll('.cart-item')
    expect(items).toHaveLength(2)
    expect(items[0].textContent).toContain('Album A')
    expect(items[0].textContent).toContain('Artist A')
    expect(items[0].textContent).toContain('$10.50')
    expect(items[1].textContent).toContain('Album B')
    expect(items[1].textContent).toContain('$20.00')
    expect(host.querySelector('#cart-panel')?.textContent).not.toContain('Album C')
    expect(button('[data-cart-remove]').getAttribute('aria-label'))
      .toBe('Remove Album A by Artist A from cart')
  })

  it('removes by ID even when titles match and allows re-adding', async () => {
    vi.mocked(axios.get).mockResolvedValue({
      data: [albums[0], { ...albums[1], title: albums[0].title }]
    })
    await mount()
    await click(addButton(0))
    await click(addButton(1))
    await click(button('.cart-trigger'))
    await click(button('[data-cart-remove]'))
    expectCount(1)
    expect(host.querySelectorAll('.cart-item')).toHaveLength(1)
    expect(host.querySelector('.cart-item')?.textContent).toContain('Artist B')
    expect(addButton(0).disabled).toBe(false)
    expect(addButton(1).disabled).toBe(true)
    await click(addButton(0))
    expectCount(2)
    expect(host.querySelectorAll('.cart-item')).toHaveLength(2)
  })

  it('keeps focus on the next, previous, then close button as items are removed', async () => {
    await mount()
    for (let index = 0; index < albums.length; index++) {
      await click(addButton(index))
    }
    await click(button('.cart-trigger'))
    button('[data-cart-remove]').focus()
    await click(button('[data-cart-remove]'))
    expect(document.activeElement?.getAttribute('aria-label')).toContain('Album B')
    const last = button('.cart-item:last-child [data-cart-remove]')
    last.focus()
    await click(last)
    expect(document.activeElement?.getAttribute('aria-label')).toContain('Album B')
    await click(button('[data-cart-remove]'))
    expectCount(0)
    expect(host.querySelector('#cart-panel')?.textContent).toContain('Your cart is empty')
    expect(document.activeElement).toBe(button('.cart-heading button'))
    expect(addButton(0).disabled).toBe(false)
    expect(addButton(1).disabled).toBe(false)
    expect(addButton(2).disabled).toBe(false)
  })

  it('preserves selections across closing, supports Escape, and resets on a fresh mount', async () => {
    await mount()
    await click(addButton(0))
    await click(button('.cart-trigger'))
    button('[data-cart-remove]').dispatchEvent(new KeyboardEvent('keydown', {
      key: 'Escape', bubbles: true, cancelable: true
    }))
    await settle()
    expect(host.querySelector('#cart-panel')).toBeNull()
    expect(document.activeElement).toBe(button('.cart-trigger'))
    expectCount(1)
    await click(button('.cart-trigger'))
    expect(host.querySelectorAll('.cart-item')).toHaveLength(1)
    await click(button('.cart-trigger'))
    expect(host.querySelector('#cart-panel')).toBeNull()
    expectCount(1)
    app?.unmount()
    host.remove()
    await mount()
    expectCount(0)
    expect(addButton(0).disabled).toBe(false)
  })

  it('preserves loading behavior while allowing access to an empty cart', async () => {
    let resolveRequest!: (value: { data: Album[] }) => void
    vi.mocked(axios.get).mockReturnValue(new Promise(resolve => { resolveRequest = resolve }))
    await mount()
    expect(host.querySelector('.loading')?.textContent).toContain('Loading albums')
    await click(button('.cart-trigger'))
    expect(host.querySelector('#cart-panel')?.textContent).toContain('Your cart is empty')
    resolveRequest({ data: albums })
    await settle()
    expect(host.querySelector('.loading')).toBeNull()
    expect(host.querySelectorAll('.album-card')).toHaveLength(3)
    expectCount(0)
  })

  it('preserves explicit API errors and retry behavior', async () => {
    const error = new Error('API unavailable')
    const log = vi.spyOn(console, 'error').mockImplementation(() => {})
    vi.mocked(axios.get).mockRejectedValueOnce(error)
    await mount()
    expect(host.querySelector('.error')?.textContent).toContain('Failed to load albums')
    expect(log).toHaveBeenCalledWith('Error fetching albums:', error)
    expectCount(0)
    await click(button('.retry-btn'))
    expect(axios.get).toHaveBeenCalledTimes(2)
    expect(host.querySelector('.error')).toBeNull()
    expect(host.querySelectorAll('.album-card')).toHaveLength(3)
    await click(addButton(0))
    expectCount(1)
  })

  it('emits the selected album from the existing card button', async () => {
    host = document.createElement('div')
    document.body.append(host)
    const onAddToCart = vi.fn()
    app = createApp(AlbumCard, { album: albums[0], onAddToCart })
    app.mount(host)
    await click(button('.btn-primary'))
    expect(onAddToCart).toHaveBeenCalledTimes(1)
    expect(onAddToCart).toHaveBeenCalledWith(albums[0])
    expect(host.querySelector('.btn-secondary')?.textContent).toBe('Preview')
  })
})
