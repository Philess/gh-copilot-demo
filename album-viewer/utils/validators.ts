// validate date from text inpu in french formatand convert it to a date object 
export const validateDate = (date: string): Date => {
    const [day, month, year] = date.split('/');
    const dateObject = new Date(`${year}-${month}-${day}`);
    return dateObject;
    }

// function that validates the format of a GUID string 
export const validateGuid = (guid: string): boolean => {
    const guidRegex = new RegExp('^[a-f0-9]{8}(-[a-f0-9]{4}){4}[a-f0-9]{8}$');
    return guidRegex.test(guid);
    }

// function that validates the format of a IPV6 address string 
export const validateIpv6 = (ipv6: string): boolean => {
    const ipv6Regex = new RegExp('^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$');
    return ipv6Regex.test(ipv6);
    }

// validate phone number from text input and extract the country code
export const validatePhoneNumber = (phoneNumber: string): string => {
    const phoneNumberRegex = new RegExp('^(\\+33|0033|0)[1-9][0-9]{8}$');
    const match = phoneNumber.match(/^(?:\+33|0033|0)([1-9])/);
    if (phoneNumberRegex.test(phoneNumber) && match !== null) {
        const countryCode = match[1];
        return countryCode;
    } else {
        // return an exception with the message "invalid country code" if the phone number is not in the correct format
        throw new Error('Invalid country code');
    }
}