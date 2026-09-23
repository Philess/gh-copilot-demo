import { describe, it, expect } from 'vitest';
import { validateDate, validateIPV6  } from './validators';

// test the validateDate function
describe('validateDate', () => {
    it('should return true for a valid date', () => {
        expect(validateDate('2023-06-01')).toBe(true);
    });

    it('should return false for an invalid date', () => {
        expect(validateDate('2023-13-01')).toBe(false);
    });

    it('should return false for an empty date string', () => {
        expect(validateDate('')).toBe(false);
    });
});

// test the validateIPV6 function
describe('validateIPV6', () => {
    it('should return true for a valid IPv6 address', () => {
        expect(validateIPV6('2001:0db8:85a3:0000:0000:8a2e:0370:7334')).toBe(true);
    });

    it('should return false for an invalid IPv6 address', () => {
        expect(validateIPV6('2001:0db8:85a3:0000:0000:8a2e:0370:7334:1234')).toBe(false);
    });

    it('should return false for an empty IPv6 address string', () => {
        expect(validateIPV6('')).toBe(false);
    });
});