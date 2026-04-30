import type { Album } from '../types/album'

function isNonEmptyString(value: unknown): value is string {
  return typeof value === 'string' && value.trim().length > 0
}

function isPositiveNumber(value: unknown): value is number {
  return typeof value === 'number' && isFinite(value) && value > 0
}

export function isValidAlbum(obj: unknown): obj is Album {
  if (typeof obj !== 'object' || obj === null) return false

  const a = obj as Record<string, unknown>

  return (
    typeof a.id === 'number' &&
    isNonEmptyString(a.title) &&
    isNonEmptyString(a.artist) &&
    isPositiveNumber(a.price) &&
    isNonEmptyString(a.image_url)
  )
}

export function isValidAlbumArray(obj: unknown): obj is Album[] {
  return Array.isArray(obj) && obj.every(isValidAlbum)
}

export function validateAlbumId(id: unknown): id is number {
  return typeof id === 'number' && Number.isInteger(id) && id > 0
}

export function validateDate(value: unknown): value is string {
  if (typeof value !== 'string' || value.trim().length === 0) return false
  const date = new Date(value)
  return !isNaN(date.getTime())
}

export function validateIPV6(value: unknown): value is string {
  if (typeof value !== 'string') return false
  const ipv6Regex = /^(?:[0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^(?:[0-9a-fA-F]{1,4}:)*::(?:[0-9a-fA-F]{1,4}:)*[0-9a-fA-F]{1,4}$|^::(?:[0-9a-fA-F]{1,4}:)*[0-9a-fA-F]{1,4}$|^(?:[0-9a-fA-F]{1,4}:)*::$|^::$/
  return ipv6Regex.test(value)
}
