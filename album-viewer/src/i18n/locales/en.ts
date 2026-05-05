const en = {
  header: {
    title: '🎵 Album Collection',
    subtitle: 'Discover amazing music albums',
  },
  loading: 'Loading albums...',
  error: 'Failed to load albums. Please make sure the API is running.',
  retry: 'Try Again',
  card: {
    addToCart: 'Add to Cart',
    preview: 'Preview',
  },
  languages: {
    en: 'English',
    fr: 'French',
    de: 'German',
  },
} as const

export default en
export type Translations = typeof en
