// function named `validateDate` which validates a date from text input in a french format and converts it to a date object
export function validateDate(input: string): Date | null {
    const regex = /^(\d{2})\/(\d{2})\/(\d{4})$/;
    const match = input.match(regex);
    if (!match) return null;

    const [, dayStr, monthStr, yearStr] = match;
    if (!dayStr || !monthStr || !yearStr) return null;

    const day = parseInt(dayStr, 10);
    const month = parseInt(monthStr, 10) - 1; // Months are 0-based in JS Date
    const year = parseInt(yearStr, 10);

    const date = new Date(year, month, day);
    if (
        date.getFullYear() !== year ||
        date.getMonth() !== month ||
        date.getDate() !== day
    ) {
        return null;
    }

    return date;
}
// function that validates the format of a GUID string.
export function validateGuid(input: string): boolean {
    const regex = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/;
    return regex.test(input);
}