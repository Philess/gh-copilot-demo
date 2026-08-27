import { describe, it, expect } from "vitest";
import { validateDate, validateGUID, validateIPV6 } from "./validators";

describe("validateDate", () => {
  it("returns a Date for a valid French-formatted date", () => {
    expect(validateDate("01/06/2023")).toEqual(new Date(2023, 5, 1));
  });

  it("returns null for an impossible date", () => {
    expect(validateDate("31/02/2023")).toBeNull();
  });

  it("returns null for malformed input", () => {
    expect(validateDate("01x/06/2023")).toBeNull();
    expect(validateDate("2023-06-01")).toBeNull();
  });
});

describe("validateGUID", () => {
  it("should return true for valid GUID", () => {
    expect(validateGUID("123e4567-e89b-12d3-a456-426614174000")).toBe(true);
  });

  it("should return false for invalid GUID", () => {
    expect(validateGUID("123e4567-e89b-12d3-a456")).toBe(false);
  });
});

describe("validateIPV6", () => {
  it("should return true for valid IPV6", () => {
    expect(validateIPV6("2001:0db8:85a3:0000:0000:8a2e:0370:7334")).toBe(true);
  });

  it("should return false for invalid IPV6", () => {
    expect(validateIPV6("2001:0db8:85a3:0000:0000:8a2e:0370")).toBe(false);
  });
});

