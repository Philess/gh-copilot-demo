import { describe, expect, it } from "vitest";
import { validateDate } from "./validators";

describe("validateDate", () => {
  it("should return a Date object for a valid French date", () => {
    const dateString = "25/12/2020";
    const result = validateDate(dateString);
    expect(result).toBeInstanceOf(Date);
    expect(result?.getFullYear()).toBe(2020);
    expect(result?.getMonth()).toBe(11); // December is month 11
    expect(result?.getDate()).toBe(25);
  });

  it("should return null for an invalid date format", () => {
    const dateString = "2020-12-25";
    const result = validateDate(dateString);
    expect(result).toBeNull();
  });
});
