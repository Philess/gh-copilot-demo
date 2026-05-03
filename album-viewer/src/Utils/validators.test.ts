import { describe, expect, it } from "vitest";
import { validateDate, validateIPV6 } from "./validators";

// test the validateDate function
describe("validateDate", () => {
  it("should return true for valid date", () => {
    expect(validateDate("2020-01-01")).toBe(true);
  });   
  it("should return false for invalid date", () => {    
    expect(validateDate("2020-13-01")).toBe(false);
    });
    it("should return false for non-date string", () => {  
    expect(validateDate("not a date")).toBe(false);
    });
    it("should return false for empty string", () => {
    expect(validateDate("")).toBe(false);
    });
});