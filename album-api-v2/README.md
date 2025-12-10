# Album API v2

API Node.js TypeScript pour la gestion d'albums musicaux - Réécriture de l'API .NET originale.

## Caractéristiques

- **TypeScript** : Code entièrement typé
- **Express.js** : Framework web rapide et minimaliste
- **CORS** : Support des requêtes cross-origin
- **Tests unitaires** : Tests complets avec Jest et Supertest
- **En mémoire** : Stockage des données en mémoire (pas de base de données requise)

## Endpoints API

| Méthode | Endpoint | Description |
|---------|----------|-------------|
| GET | `/albums` | Récupère tous les albums |
| GET | `/albums/:id` | Récupère un album par ID |
| POST | `/albums` | Crée un nouvel album |
| PUT | `/albums/:id` | Met à jour un album existant |
| DELETE | `/albums/:id` | Supprime un album |

## Installation

```bash
npm install
```

## Scripts disponibles

```bash
# Build du projet TypeScript
npm run build

# Démarrer l'application (production)
npm start

# Démarrer en mode développement
npm run dev

# Lancer les tests
npm test

# Lancer les tests en mode watch
npm run test:watch

# Générer un rapport de couverture
npm run test:coverage
```

## Utilisation

L'API démarre par défaut sur le port 3000 :

```
http://localhost:3000
```

### Exemples de requêtes

**Récupérer tous les albums :**
```bash
curl http://localhost:3000/albums
```

**Créer un album :**
```bash
curl -X POST http://localhost:3000/albums \
  -H "Content-Type: application/json" \
  -d '{
    "title": "My Album",
    "artist": "My Artist",
    "price": 15.99,
    "image_url": "https://example.com/album.jpg"
  }'
```

**Mettre à jour un album :**
```bash
curl -X PUT http://localhost:3000/albums/1 \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Updated Title",
    "price": 19.99
  }'
```

**Supprimer un album :**
```bash
curl -X DELETE http://localhost:3000/albums/1
```

## Structure du projet

```
album-api-v2/
├── src/
│   ├── controllers/
│   │   ├── albumController.ts
│   │   └── albumController.test.ts
│   ├── models/
│   │   ├── album.ts
│   │   └── album.test.ts
│   ├── routes/
│   │   └── albumRoutes.ts
│   └── server.ts
├── dist/                 # Fichiers compilés (générés après build)
├── package.json
├── tsconfig.json
├── jest.config.js
└── README.md
```

## Données d'exemple

L'API contient 6 albums préchargés avec les mêmes données que l'API .NET originale :

1. "You, Me and an App Id" - Daprize
2. "Seven Revision Army" - The Blue-Green Stripes
3. "Scale It Up" - KEDA Club
4. "Lost in Translation" - MegaDNS
5. "Lock Down Your Love" - V is for VNET
6. "Sweet Container O' Mine" - Guns N Probeses

## Compatibilité

Cette API est compatible avec l'application Vue.js `album-viewer` existante et peut être utilisée comme remplacement direct de l'API .NET.
