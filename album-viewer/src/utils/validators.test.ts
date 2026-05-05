import { describe, it, expect } from 'vitest';
import { validateDate, validateIPV6 } from "./validators";

describe('validateDate', () => {
    it('returns a Date object for valid DD/MM/YYYY', () => {
        const result = validateDate('25/12/2023');
        expect(result).toBeInstanceOf(Date);
        expect(result?.getFullYear()).toBe(2023);
        expect(result?.getMonth()).toBe(11); // December is 11
        expect(result?.getDate()).toBe(25);
    });

    it('returns a Date object for valid DD-MM-YYYY', () => {
        const result = validateDate('01-01-2020');
        expect(result).toBeInstanceOf(Date);
        expect(result?.getFullYear()).toBe(2020);
        expect(result?.getMonth()).toBe(0); // January is 0
        expect(result?.getDate()).toBe(1);
    });

    it('returns a Date object for valid DD.MM.YYYY', () => {
        const result = validateDate('15.07.1999');
        expect(result).toBeInstanceOf(Date);
        expect(result?.getFullYear()).toBe(1999);
        expect(result?.getMonth()).toBe(6); // July is 6
        expect(result?.getDate()).toBe(15);
    });

    it('returns null for invalid day', () => {
        expect(validateDate('32/01/2023')).toBeNull();
        expect(validateDate('00/01/2023')).toBeNull();
    });

    it('returns null for invalid month', () => {
        expect(validateDate('10/13/2023')).toBeNull();
        expect(validateDate('10/00/2023')).toBeNull();
    });

    it('returns null for invalid year', () => {
        expect(validateDate('10/10/abcd')).toBeNull();
        expect(validateDate('10/10/20')).toBeNull();
    });

    it('returns null for invalid format', () => {
        expect(validateDate('2023/12/25')).toBeNull();
        expect(validateDate('25-12/2023')).toBeNull();
        expect(validateDate('')).toBeNull();
        expect(validateDate('25/12/2023/extra')).toBeNull();
    });

    it('handles leap years correctly', () => {
        expect(validateDate('29/02/2020')).toBeInstanceOf(Date); // Leap year
        expect(validateDate('29/02/2021')).toBeNull(); // Not a leap year
    });

    it('trims whitespace', () => {
        const result = validateDate('  05/06/2022  ');
        expect(result).toBeInstanceOf(Date);
        expect(result?.getFullYear()).toBe(2022);
        expect(result?.getMonth()).toBe(5);
        expect(result?.getDate()).toBe(5);
    });

    it('returns null for non-string input', () => {
        // @ts-expect-error
        expect(validateDate(null)).toBeNull();
        // @ts-expect-error
        expect(validateDate(undefined)).toBeNull();
        // @ts-expect-error
        expect(validateDate(12345)).toBeNull();
    });
});
