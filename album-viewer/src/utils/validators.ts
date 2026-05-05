/**
 * Validates a date from text input in French format and converts it to a Date object.
 * Supports formats: DD/MM/YYYY, DD-MM-YYYY, and DD.MM.YYYY
 * 
 * @param dateString - The date string in French format
 * @returns A Date object if valid, null if invalid
 * 
 * @example
 * validateDate('25/12/2023') // Returns Date object for December 25, 2023
 * validateDate('32/13/2023') // Returns null (invalid date)
 */
export function validateDate(dateString: string): Date | null {
  if (!dateString || typeof dateString !== 'string') {
    return null;
  }

  // Remove leading and trailing whitespace
  const trimmed = dateString.trim();

  // Match French date formats: DD/MM/YYYY, DD-MM-YYYY, or DD.MM.YYYY
  const dateRegex = /^(\d{1,2})[/\-.](\d{1,2})[/\-.](\d{4})$/;
  const match = trimmed.match(dateRegex);

  if (!match) {
    return null;
  }

  const day = parseInt(match[1], 10);
  const month = parseInt(match[2], 10);
  const year = parseInt(match[3], 10);

  // Validate month
  if (month < 1 || month > 12) {
    return null;
  }

  // Validate day (accounting for different month lengths)
  const daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

  // Account for leap years
  if (isLeapYear(year)) {
    daysInMonth[1] = 29;
  }

  if (day < 1 || day > daysInMonth[month - 1]) {
    return null;
  }

  // Create date object (JavaScript months are 0-indexed)
  const date = new Date(year, month - 1, day);

  // Additional validation to catch edge cases
  if (date.getFullYear() !== year || date.getMonth() !== month - 1 || date.getDate() !== day) {
    return null;
  }

  return date;
}

/**
 * Helper function to determine if a year is a leap year
 * 
 * @param year - The year to check
 * @returns True if the year is a leap year, false otherwise
 */
function isLeapYear(year: number): boolean {
  return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
}

/**
 * Validates the format of a GUID (Globally Unique Identifier) string.
 * Accepts both with and without hyphens, and case-insensitive.
 * 
 * Valid formats:
 * - XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX (standard)
 * - XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX (without hyphens)
 * 
 * @param guidString - The GUID string to validate
 * @returns True if the string is a valid GUID format, false otherwise
 * 
 * @example
 * validateGuid('550e8400-e29b-41d4-a716-446655440000') // Returns true
 * validateGuid('550e8400e29b41d4a716446655440000') // Returns true
 * validateGuid('invalid-guid') // Returns false
 */
export function validateGuid(guidString: string): boolean {
  if (!guidString || typeof guidString !== 'string') {
    return false;
  }

  // GUID format: 8-4-4-4-12 hexadecimal digits with hyphens
  const guidWithHyphens = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
  
  // GUID format: 32 hexadecimal digits without hyphens
  const guidWithoutHyphens = /^[0-9a-f]{32}$/i;

  return guidWithHyphens.test(guidString.trim()) || guidWithoutHyphens.test(guidString.trim());
}

/**
 * Validates the format of an IPv6 address string.
 * Accepts standard IPv6 notation, including shorthand (::) and leading zero suppression.
 *
 * @param ipv6String - The IPv6 address string to validate
 * @returns True if the string is a valid IPv6 address format, false otherwise
 *
 * @example
 * validateIPV6('2001:0db8:85a3:0000:0000:8a2e:0370:7334') // Returns true
 * validateIPV6('2001:db8::8a2e:370:7334') // Returns true
 * validateIPV6('invalid-ipv6') // Returns false
 */
export function validateIPV6(ipv6String: string): boolean {
  if (!ipv6String || typeof ipv6String !== 'string') {
    return false;
  }

  // IPv6 regex covers full, shorthand, and mixed notation
  // Reference: https://stackoverflow.com/a/17871737
  const ipv6Regex = /^(([0-9a-fA-F]{1,4}:){7}([0-9a-fA-F]{1,4}|:)|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:))(%.+)?$/;

  return ipv6Regex.test(ipv6String.trim());
}

