import {describe, expect, it} from 'vitest';
import {validateDate, validateGuid} from './validator';
// test the validateDate function
describe('validateDate', () => {
    it('should return a Date object for valid date input', () => {
        const input = '25/12/2020';
        const result = validateDate(input);
        expect(result).toBeInstanceOf(Date);
        expect(result?.getDate()).toBe(25);
        expect(result?.getMonth()).toBe(11); // Months are 0-based
        expect(result?.getFullYear()).toBe(2020);
    });

    it('should return null for invalid date format', () => {
        const input = '2020-12-25';
        const result = validateDate(input);
        expect(result).toBeNull();
    });

    it('should return null for non-existent date', () => {
        const input = '31/02/2020';
        const result = validateDate(input);
        expect(result).toBeNull();
    });
});

// test the validateGuid function
describe('validateGuid', () => {
    it('should return true for valid GUID input', () => {
        const input = '123e4567-e89b-12d3-a456-426614174000';
        const result = validateGuid(input);
        expect(result).toBe(true);
    });

    it('should return false for invalid GUID format', () => {
        const input = '123e4567-e89b-12d3-a456-42661417400Z';
        const result = validateGuid(input);
        expect(result).toBe(false);
    });

    it('should return false for non-GUID input', () => {
        const input = 'not-a-guid';
        const result = validateGuid(input);
        expect(result).toBe(false);
    });
});