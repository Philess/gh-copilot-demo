// function named `validateDate` which validates a date from text input in a french format and converts it to a date object.
export function validateDate(input: string): Date | null {
    // French date format: DD/MM/YYYY
    const regex = /^(\d{2})\/(\d{2})\/(\d{4})$/;
    const match = input.match(regex);
    if (!match) {
        return null;
    }
    const day = parseInt(match[1], 10);
    const month = parseInt(match[2], 10) - 1; // Months are 0-based in JS Date
    const year = parseInt(match[3], 10);
    const date = new Date(year, month, day);
    // Check if the date is valid
    if (date.getFullYear() !== year || date.getMonth() !== month || date.getDate() !== day) {
        return null;
    }
    return date;
}

// function that validates the format of a GUID string.
export function validateGUID(input: string): boolean {
    const regex = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/;
    return regex.test(input);
}

// function that validates the format of a IPV6 address string and is named `validateIPV6`.
export function validateIPV6(input: string): boolean {
    const regex = /^(?:[a-fA-F0-9]{1,4}:){7}[a-fA-F0-9]{1,4}$/;
    return regex.test(input);
}
