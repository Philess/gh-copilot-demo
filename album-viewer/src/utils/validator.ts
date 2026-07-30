export function validateDate(input: string): Date | null {
  if (typeof input !== 'string') {
    return null;
  }

  const trimmedInput = input.trim();
  const match = /^([0-9]{1,2})\/([0-9]{1,2})\/([0-9]{4})$/.exec(trimmedInput);

  if (!match) {
    return null;
  }

  const day = Number(match[1]);
  const month = Number(match[2]);
  const year = Number(match[3]);

  if (!Number.isInteger(day) || !Number.isInteger(month) || !Number.isInteger(year)) {
    return null;
  }

  if (month < 1 || month > 12) {
    return null;
  }

  const candidateDate = new Date(year, month - 1, day);

  if (
    candidateDate.getFullYear() !== year ||
    candidateDate.getMonth() !== month - 1 ||
    candidateDate.getDate() !== day
  ) {
    return null;
  }

  return candidateDate;
}
