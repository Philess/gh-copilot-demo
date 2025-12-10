# Album API v2

API Node.js/TypeScript pour gérer des albums musicaux. Cette API est une réécriture de l'API .NET `albums-api`.

## Prérequis

- Node.js 18+
- npm

## Installation

```bash
npm install
```

## Scripts disponibles

| Script | Description |
|--------|-------------|
| `npm run dev` | Démarre le serveur en mode développement avec hot-reload |
| `npm run build` | Compile le TypeScript vers JavaScript |
| `npm start` | Démarre le serveur de production (nécessite un build préalable) |
| `npm test` | Exécute les tests unitaires |

## Démarrage rapide

```bash
# Installation des dépendances
npm install

# Build du projet
npm run build

# Démarrer le serveur sur le port 3000
npm start
```

L'API sera accessible sur `http://localhost:3000`

## Endpoints

| Méthode | Route | Description |
|---------|-------|-------------|
| `GET` | `/` | Message d'accueil |
| `GET` | `/albums` | Liste tous les albums |
| `GET` | `/albums/:id` | Récupère un album par ID |
| `POST` | `/albums` | Crée un nouvel album |
| `PUT` | `/albums/:id` | Met à jour un album |
| `DELETE` | `/albums/:id` | Supprime un album |

## Modèle Album

```typescript
interface Album {
  id: number;
  title: string;
  artist: string;
  price: number;
  image_url: string;
}
```

## Exemples d'utilisation

### Lister tous les albums

```bash
curl http://localhost:3000/albums
```

### Récupérer un album par ID

```bash
curl http://localhost:3000/albums/1
```

### Créer un nouvel album

```bash
curl -X POST http://localhost:3000/albums \
  -H "Content-Type: application/json" \
  -d '{
    "title": "New Album",
    "artist": "Artist Name",
    "price": 12.99,
    "image_url": "https://example.com/image.jpg"
  }'
```

### Mettre à jour un album

```bash
curl -X PUT http://localhost:3000/albums/1 \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Updated Title",
    "price": 14.99
  }'
```

### Supprimer un album

```bash
curl -X DELETE http://localhost:3000/albums/1
```

## Tests

```bash
npm test
```

Les tests couvrent :
- ✅ Route racine (message d'accueil)
- ✅ Liste de tous les albums
- ✅ Récupération d'un album par ID
- ✅ Création d'un nouvel album
- ✅ Mise à jour d'un album existant
- ✅ Suppression d'un album
- ✅ Gestion des erreurs (404, 400)

## Structure du projet

```
album-api-v2/
├── package.json
├── tsconfig.json
├── vitest.config.ts
├── dist/                    # Code compilé (généré)
└── src/
    ├── index.ts             # Point d'entrée
    ├── app.ts               # Configuration Express
    ├── models/
    │   └── album.ts         # Interface Album
    ├── data/
    │   └── albums.ts        # Données en mémoire
    ├── routes/
    │   └── albums.ts        # Routes CRUD
    └── __tests__/
        └── albums.test.ts   # Tests unitaires
```

## Configuration

| Variable d'environnement | Description | Défaut |
|--------------------------|-------------|--------|
| `PORT` | Port d'écoute du serveur | `3000` |

## Compatibilité avec album-viewer

Cette API est compatible avec l'application Vue.js `album-viewer`. Le proxy Vite est configuré pour rediriger les appels `/albums` vers `http://localhost:3000`.
