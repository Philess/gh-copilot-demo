import { describe, expect, it } from "vitest";
import { validateDate } from "./validators";

describe("validateDate", () => {
  it("should return true for a valid date", () => {
    expect(validateDate("2023-01-01")).toBe(true);
  });

  it("should return false for an invalid date", () => {
    expect(validateDate("2023-13-01")).toBe(false);
  });
});