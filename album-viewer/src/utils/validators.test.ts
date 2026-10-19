import {describe, it, expect} from 'vitest';
import { validateDate } from './validators';
import { validateIPV6 } from './validators';

// test the validateDate function
describe('validateDate', () => {
  it('should return true for valid dates', () => {
    expect(validateDate('2023-01-01')).toBe(true);
    expect(validateDate('2020-02-29')).toBe(true);
  });

  it('should return false for invalid dates', () => {
    expect(validateDate('2023-02-30')).toBe(false);
    expect(validateDate('not-a-date')).toBe(false);
  });

  it('should return false for invalid date formats', () => {
    expect(validateDate('2023/01/01')).toBe(false);
    expect(validateDate('01-01-2023')).toBe(false);
  });
});

// test the validateIPV6 function
describe('validateIPV6', () => {
  it('should return true for valid IPv6 addresses', () => {
    expect(validateIPV6('2001:0db8:85a3:0000:0000:8a2e:0370:7334')).toBe(true);
    expect(validateIPV6('::1')).toBe(true);
  });

  it('should return false for invalid IPv6 addresses', () => {
    expect(validateIPV6('2001:0db8:85a3:0000:0000:8a2e:0370:7334:1234')).toBe(false);
    expect(validateIPV6('not-an-ipv6')).toBe(false);
  });
});