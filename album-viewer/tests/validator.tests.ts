import { describe, it } from 'mocha';
import { expect } from 'chai';

import { validateDate, validateGuid, validateIpv6, validatePhoneNumber } from '../utils/validators';

// test the validateDate function
describe('validateDate', () => {
    it('should return a date object from a string', () => {
        const date = '31/12/2020';
        const dateObject = new Date('2020-12-31');
        expect(validateDate(date)).to.deep.equal(dateObject);
    });

    it('should throw an error if the date is not in the correct format', () => {
        const date = '31-12-2020';
        expect(() => validateDate(date)).to.throw('Invalid date format');
    });

    // add more tests when date is empty
    it ('should throw an error if the date is empty', () => {
        const date = '';
        expect(() => validateDate(date)).to.throw('Invalid date format');
    });
});

// test the validateGuid function
describe('validateGuid', () => {
    it('should return true if the GUID is in the correct format', () => {
        const guid = '123e4567-e89b-12d3-a456-426614174000';
        expect(validateGuid(guid)).to.equal(true);
    });

    it('should return false if the GUID is not in the correct format', () => {
        const guid = '123e4567-e89b-12d3-a456-4266141K';
        expect(validateGuid(guid)).to.equal(false);
    });
});

// test the validateIpv6 function
describe('validateIpv6', () => {
    it('should return true if the IPV6 address is in the correct format', () => {
        const ipv6 = '2001:0db8:85a3:0000:0000:8a2e:0370:7334';
        expect(validateIpv6(ipv6)).to.equal(true);
    });

    it('should return false if the IPV6 address is not in the correct format', () => {
        const ipv6 = '192.168.0.0';
        expect(validateIpv6(ipv6)).to.equal(false);
    });
});

// test the validatePhoneNumber function
describe('validatePhoneNumber', () => {
    it('should return the country code if the phone number is in the correct format', () => {
        const phoneNumber = '+33612345678';
        expect(validatePhoneNumber(phoneNumber)).to.equal('+33');
    });

    it('should throw an error if the phone number is not in the excepted format', () => {
        const phoneNumber = '0612345678';
        expect(() => validatePhoneNumber(phoneNumber)).to.throw('Invalid country code');
    });
});