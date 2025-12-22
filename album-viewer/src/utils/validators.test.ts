import { describe, expect, it } from 'vitest';
import { validateDate, validateIPV6 } from './validators';
describe('validateDate', () => {
    it('valid dates', () => {
        const validDates = [
            { input: '01/31/2020', expected: new Date(2020, 0, 31) },
            { input: '31-12-99', expected: new Date(2099, 11, 31) },
            { input: '15.08.1947', expected: new Date(1947, 7, 15) },
            { input: '  5  6  2021  ', expected: new Date(2021, 5, 5) }
        ];

        validDates.forEach(({ input, expected }) => {
            const result = validateDate(input);
            expect(result).not.toBeNull();
            expect(result?.getFullYear()).toBe(expected.getFullYear());
            expect(result?.getMonth()).toBe(expected.getMonth());
            expect(result?.getDate()).toBe(expected.getDate());
        });
    });

    it('invalid dates', () => {
        const invalidDates = [
            '02/30/2020',
            '31-04-2021',
            '15.13.1947',
            'invalid-date',
            '',
            '  '
        ];

        invalidDates.forEach(input => {
            const result = validateDate(input);
            expect(result).toBeNull();
        });
    });
});
describe('validateIPV6', () => {
    it('valid addresses', () => {
        const validAddresses = [            
            '2001:0db8:85a3:0000:0000:8a2e:0370:7334',
            '2001:db8:85a3:0:0:8a2e:370:7334',
            '2001:db8::1',
            'fe80::1%eth0',
            '::ffff::192.168.1.1'
        ];

        validAddresses.forEach(addr => {
            expect(validateIPV6(addr)).toBe(true);
        });
    });

    it('invalid addresses', () => {
        const invalidAddresses = [
            'invalid-ip',
            '256.1.1.1',
            '::ffff::invalid'
        ];

        invalidAddresses.forEach(addr => {
            expect(validateIPV6(addr)).toBe(false);
        });
    });

});