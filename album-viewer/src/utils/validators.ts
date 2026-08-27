// function named `validateDate` which validates a date from text input in a french format and 
// converts it to a date object.
export function validateDate(input: string): Date | null {
    const match = /^(\d{2})\/(\d{2})\/(\d{4})$/.exec(input);
    if (!match) {
        return null;
    }
    const day = Number(match[1]);
    const month = Number(match[2]) - 1;
    const year = Number(match[3]);
    const date = new Date(year, month, day);
    if (date.getFullYear() !== year || date.getMonth() !== month || date.getDate() !== day) {
        return null;
    }
    return date;
}

// function that validates the format of a GUID string.
export function validateGUID(input: string): boolean {
    const guidRegex = /^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$/;
    return guidRegex.test(input);
}

// function that validates the format of a IPV6 address string and is named `validateIPV6`.
export function validateIPV6(input: string): boolean {
    const ipv6Regex = /^(?:[a-fA-F0-9]{1,4}:){7}[a-fA-F0-9]{1,4}$/;
    return ipv6Regex.test(input);
}