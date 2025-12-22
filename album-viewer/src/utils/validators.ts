// function named `validateDate` which validates a date from text input in a french format and converts it to a date object.

/**
 * Validate a date provided as text in a French-style format and return a Date object.
 * Supported formats: `DD/MM/YYYY`, `D/M/YYYY`, with separators `/`, `-`, `.`, or spaces.
 * Accepts 2- or 4-digit years (2-digit years are interpreted as 2000+year).
 * Returns a `Date` if valid, otherwise `null`.
 */
export function validateDate(input: string): Date | null {
	if (!input || typeof input !== 'string') return null;

	const trimmed = input.trim();
	// Accept separators: slash, dash, dot or space
	const re = /^(\d{1,2})[\/\-\.\s](\d{1,2})[\/\-\.\s](\d{2,4})$/;
	const m = trimmed.match(re);
	if (!m) return null;

	let day = Number(m[1]);
	let month = Number(m[2]);
	let year = Number(m[3]);

	if (String(m[3]).length === 2) {
		year = 2000 + year;
	}

	if (month < 1 || month > 12) return null;

	const daysInMonth = (y: number, mth: number) => {
		return new Date(y, mth, 0).getDate();
	};

	const maxDay = daysInMonth(year, month);
	if (day < 1 || day > maxDay) return null;

	const date = new Date(year, month - 1, day);
	// Ensure that the Date object matches the components (guards against JS auto-correction)
	if (date.getFullYear() !== year || date.getMonth() !== month - 1 || date.getDate() !== day) {
		return null;
	}

	return date;
}

export default validateDate;

/**
 * Validate a GUID/UUID string.
 * Accepts standard 8-4-4-4-12 hex format, optionally wrapped in `{}` or `()`.
 * Examples: `01234567-89ab-cdef-0123-456789abcdef`, `{01234567-89ab-cdef-0123-456789abcdef}`
 */
export function validateGuid(input: string): boolean {
	if (!input || typeof input !== 'string') return false;
	const trimmed = input.trim();
	const re = /^[{(]?([0-9a-fA-F]{8})-([0-9a-fA-F]{4})-([0-9a-fA-F]{4})-([0-9a-fA-F]{4})-([0-9a-fA-F]{12})[)}]?$/;
	return re.test(trimmed);
}

/**
 * Validate an IPv6 address string.
 * Accepts full, compressed (`::`), zone-indexed (e.g. `fe80::1%eth0`) and IPv4-embedded forms.
 */
export function validateIPV6(input: string): boolean {
	if (!input || typeof input !== 'string') return false;
	const s = input.trim();
	if (s.length === 0) return false;

	const ipv6Regex = new RegExp('^(?:' +
		// 1: Full form
		'([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}|' +
		// 2: :: compression variants
		'([0-9A-Fa-f]{1,4}:){1,7}:|' +
		'([0-9A-Fa-f]{1,4}:){1,6}:[0-9A-Fa-f]{1,4}|' +
		'([0-9A-Fa-f]{1,4}:){1,5}(:[0-9A-Fa-f]{1,4}){1,2}|' +
		'([0-9A-Fa-f]{1,4}:){1,4}(:[0-9A-Fa-f]{1,4}){1,3}|' +
		'([0-9A-Fa-f]{1,4}:){1,3}(:[0-9A-Fa-f]{1,4}){1,4}|' +
		'([0-9A-Fa-f]{1,4}:){1,2}(:[0-9A-Fa-f]{1,4}){1,5}|' +
		'[0-9A-Fa-f]{1,4}:((:[0-9A-Fa-f]{1,4}){1,6})|' +
		':((:[0-9A-Fa-f]{1,4}){1,7}|:)|' +
		// 3: Link-local with zone index
		'fe80:(:[0-9A-Fa-f]{0,4}){0,4}%[0-9a-zA-Z]{1,}|' +
		// 4: IPv4-mapped / IPv4-embedded
		'::ffff:(?:' +
			'(?:25[0-5]|2[0-4]\d|1?\d?\d)\.' +
			'(?:25[0-5]|2[0-4]\d|1?\d?\d)\.' +
			'(?:25[0-5]|2[0-4]\d|1?\d?\d)\.' +
			'(?:25[0-5]|2[0-4]\d|1?\d?\d)' +
		')|' +
		'([0-9A-Fa-f]{1,4}:){1,4}:' +
			'(?:' +
				'(?:25[0-5]|2[0-4]\d|1?\d?\d)\.' +
				'(?:25[0-5]|2[0-4]\d|1?\d?\d)\.' +
				'(?:25[0-5]|2[0-4]\d|1?\d?\d)\.' +
				'(?:25[0-5]|2[0-4]\d|1?\d?\d)' +
			')' +
		')$');

	return ipv6Regex.test(s);
}
