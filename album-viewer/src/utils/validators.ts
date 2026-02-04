// function named `validateDate` which validates a date from text input in a french format and converts it to a date object.

export function validateDate(dateString: string): Date | null {
  // Regular expression to match French date format (DD/MM/YYYY)
  const frenchDateRegex = /^(\d{2})\/(\d{2})\/(\d{4})$/;
  const match = frenchDateRegex.exec(dateString);

  if (!match) {
    return null; // Invalid format
  }

  const day = Number.parseInt(match[1]!, 10);
  const month = Number.parseInt(match[2]!, 10) - 1;
  const year = Number.parseInt(match[3]!, 10);

  const date = new Date(year, month, day);

  // Check if the date is valid
  if (
    date.getFullYear() !== year ||
    date.getMonth() !== month ||
    date.getDate() !== day
  ) {
    return null; // Invalid date
  }

  return date;
}
