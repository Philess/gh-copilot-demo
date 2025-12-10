# Album API v2 (Node.js + TypeScript)

Réécriture de l'API .NET (`albums-api`) en Node.js/TypeScript. Fournit des routes CRUD pour gérer des albums de musique. Données conservées en mémoire (pas de base de données).

## Prérequis
- Node.js 18+ et npm
- Port `3000` libre (utilisé par l'API)

## Installation

```bash
cd album-api-v2
npm install
```

## Scripts

```bash
# Démarrage en dev (ts-node + nodemon)
npm run dev

# Build TypeScript vers dist/
npm run build

# Lancer depuis dist/ sur port 3000
npm start

# Lancer les tests Jest
npm test
```

## Configuration
- L'application écoute sur `http://localhost:3000` pour s'aligner avec le proxy de l'app Vue (`album-viewer`).
- CORS ouvert en dev pour faciliter les tests.

## Routes
- `GET /` → renvoie le texte: `Hit the /albums endpoint to retrieve a list of albums!`
- `GET /albums` → liste de tous les albums
- `GET /albums/:id` → détail d'un album (par ID)
- `POST /albums` → ajoute un nouvel album
- `PUT /albums/:id` → met à jour un album existant
- `DELETE /albums/:id` → supprime un album

## Schéma Album (TypeScript)
```ts
export interface Album {
  id: number;
  title: string;
  artist: string;
  year: number;
  genre: string;
}
```

## Données d'exemple
Les 6 albums sont initialisés en mémoire, identiques à ceux de l'API .NET existante.

## Intégration avec l'app Vue
- Le dev server Vite (dans `album-viewer`) proxy les appels `/albums` vers `http://localhost:3000`.
- Assurez-vous que l'API est démarrée avant de lancer `album-viewer`.

## Dépannage
- Conflit de port: changez le port via la variable d'environnement `PORT=3000` (si supporté) ou modifiez la config du serveur.
- CORS: si vous testez depuis un autre client que `album-viewer`, vérifiez les en-têtes CORS.

## Prochaines étapes
- Implémenter la logique des routes et les tests unitaires.
- Ajouter une validation basique des payloads (optionnel).
