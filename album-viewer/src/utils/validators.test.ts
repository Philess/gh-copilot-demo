import { describe, expect, it } from 'vitest';
import { validateDate } from './validator';

describe('validateDate', () => {
  it('should return a valid Date object for a valid date string', () => {
    const result = validateDate('12/25/2020');
    expect(result).toBeInstanceOf(Date);
  });
    it('should return null for a date string with invalid month', () => {
    const result = validateDate('12/32/2020');
    expect(result).toBeNull();
  });



});


