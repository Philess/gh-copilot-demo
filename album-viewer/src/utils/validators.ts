/**
 * Validates a date string in French format (dd/mm/yyyy) and converts it to a Date object
 * @param dateString - The date string to validate in format dd/mm/yyyy
 * @returns The Date object if valid, or null if invalid
 */
export function validateDate(dateString: string): Date | null {
  // French date format: dd/mm/yyyy
  const frenchDateRegex = /^(\d{2})\/(\d{2})\/(\d{4})$/;
  const match = dateString.trim().match(frenchDateRegex);

  if (!match) {
    return null;
  }

  const day = parseInt(match[1], 10);
  const month = parseInt(match[2], 10);
  const year = parseInt(match[3], 10);

  // Validate month and day ranges
  if (month < 1 || month > 12 || day < 1 || day > 31) {
    return null;
  }

  // Create date (JavaScript months are 0-indexed)
  const date = new Date(year, month - 1, day);

  // Verify the date is valid (handles invalid dates like Feb 30)
  if (
    date.getFullYear() !== year ||
    date.getMonth() !== month - 1 ||
    date.getDate() !== day
  ) {
    return null;
  }

  return date;
}

/**
 * Validates the format of a GUID string
 * @param guid - The GUID string to validate
 * @returns true if the string is a valid GUID format, false otherwise
 */
export function validateGUID(guid: string): boolean {
  // Standard GUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
  const guidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
  return guidRegex.test(guid.trim());
}
