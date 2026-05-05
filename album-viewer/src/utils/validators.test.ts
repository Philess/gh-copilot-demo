import { describe, expect, it } from "vitest";
import { validateDate, validateIPV6 } from "../validators";

// test the validateDate function   
describe("validateDate", () => {
    it("should return a Date object for valid date string", () => {
        const dateString = "25/12/2020";
        const result = validateDate(dateString);
        expect(result).toBeInstanceOf(Date);
        expect(result?.getDate()).toBe(25);
        expect(result?.getMonth()).toBe(11); // Months are zero-based
        expect(result?.getFullYear()).toBe(2020);
    });

    it("should return null for invalid date format", () => {
        const dateString = "2020-12-25";
        const result = validateDate(dateString);
        expect(result).toBeNull();
    });

    it("should return null for invalid date values", () => {
        const dateString = "31/02/2020";
        const result = validateDate(dateString);
        expect(result).toBeNull();
    });
});

// test the validateIPV6 function
describe("validateIPV6", () => {
    it("should return true for valid IPV6 address", () => {
        const ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334";
        const result = validateIPV6(ipv6);
        expect(result).toBe(true);
    });

    it("should return false for invalid IPV6 address", () => {
        const ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334:1234";
        const result = validateIPV6(ipv6);
        expect(result).toBe(false);
    });

});