import {describe, expect, it} from "vitest";
//import {validateAlbum} from "./validators";
import { validateDate, validateIPV6 } from "./validators";
describe("validateDate", () => {
    it("should return a Date object for a valid date string", () => {
        const dateString = "25/12/2020";
        const result = validateDate(dateString);
        expect(result).toBeInstanceOf(Date);
        expect(result?.getDate()).toBe(25);
        expect(result?.getMonth()).toBe(11); // Months are zero-based
        expect(result?.getFullYear()).toBe(2020);
    });

    it("should return null for an invalid date string", () => {
        const dateString = "31/02/2020"; // Invalid date
        const result = validateDate(dateString);
        expect(result).toBeNull();
    });

    it("should return null for a string that does not match the format", () => {
        const dateString = "2020-12-25"; // Wrong format
        const result = validateDate(dateString);
        expect(result).toBeNull();
    });
});

describe("validateIPV6", () => {
    it("should return true for a valid IPV6 address", () => {
        const ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370:7334";
        expect(validateIPV6(ipv6)).toBe(true);
    });

    it("should return false for an invalid IPV6 address", () => {
        const ipv6 = "2001:0db8:85a3:0000:0000:8a2e:0370"; // Missing one segment
        expect(validateIPV6(ipv6)).toBe(false);
    });

    it("should return false for a string that does not match the format", () => {
        const ipv6 = "  2001:0db8:85a3:0000:0000:8a2e:0370:733g"; // Invalid character
        expect(validateIPV6(ipv6)).toBe(false);
    });

    it("should return false for an empty string", () => {
        const ipv6 = "";
        expect(validateIPV6(ipv6)).toBe(false);
    });
});

/*describe("validateAlbum", () => {
    it("should return true for a valid album", () => {
        const album = {
            id: 1,
            title: "You, Me and an App Id",
            artist: "Daprize",
            year: 2020,
            price: 10.99,
            image_url: "https://aka.ms/albums-daprlogo"
        };

        expect(validateAlbum(album)).toBe(true);
    });

    it("should return false for an album with missing fields", () => {
        const album = {
            id: 1,
            title: "You, Me and an App Id",
            artist: "Daprize",
            price: 10.99,
            image_url: "https://aka.ms/albums-daprlogo"
        };

        expect(validateAlbum(album)).toBe(false);
    });

    it("should return false for an album with invalid price", () => {
        const album = {
            id: 1,
            title: "You, Me and an App Id",
            artist: "Daprize",
            year: 2020,
            price: -10.99,
            image_url: "https://aka.ms/albums-daprlogo"
        };

        expect(validateAlbum(album)).toBe(false);
    });

    it("should return false for an album with invalid year", () => {
        const album = {
            id: 1,
            title: "You, Me and an App Id",
            artist: "Daprize",
            year: 1800,
            price: 10.99,
            image_url: "https://aka.ms/albums-daprlogo"
        };

        expect(validateAlbum(album)).toBe(false);
    });
});*/

