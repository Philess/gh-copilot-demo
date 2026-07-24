import { describe, it, expect } from 'vitest'
import { validateDate, validateGUID } from './validators'

describe('validators', () => {
  describe('validateDate', () => {
    it('should validate correct French date format', () => {
      const result = validateDate('25/12/2023')
      expect(result).toEqual(new Date(2023, 11, 25))
    })

    it('should return null for invalid date format', () => {
      const result = validateDate('2023-12-25')
      expect(result).toBeNull()
    })

    it('should return null for invalid day', () => {
      const result = validateDate('32/12/2023')
      expect(result).toBeNull()
    })
  })

  describe('validateGUID', () => {
    it('should validate correct GUID format', () => {
      const result = validateGUID('550e8400-e29b-41d4-a716-446655440000')
      expect(result).toBe(true)
    })

    it('should return false for invalid GUID format', () => {
      const result = validateGUID('not-a-guid')
      expect(result).toBe(false)
    })
  })
})
