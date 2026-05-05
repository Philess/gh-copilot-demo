import type { Translations } from './en'

const fr: Translations = {
  header: {
    title: '🎵 Collection d\'Albums',
    subtitle: 'Découvrez des albums musicaux incroyables',
  },
  loading: 'Chargement des albums...',
  error: 'Impossible de charger les albums. Veuillez vérifier que l\'API est en cours d\'exécution.',
  retry: 'Réessayer',
  card: {
    addToCart: 'Ajouter au panier',
    preview: 'Aperçu',
  },
  cart: {
    title: 'Mon panier',
    empty: 'Votre panier est vide.',
    remove: 'Supprimer',
    total: 'Total',
    itemCount: '{count} article(s)',
    inCart: 'Dans le panier',
  },
  languages: {
    en: 'Anglais',
    fr: 'Français',
    de: 'Allemand',
  },
}

export default fr
