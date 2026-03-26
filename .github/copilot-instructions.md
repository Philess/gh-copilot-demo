# Copilot Instructions — gh-copilot-demo

## Project Overview

This repository is a demo application composed of two microservices:

- **`albums-api`** — ASP.NET Core 8 REST API that exposes an album catalog. Uses Dapr for state management (state store: `statestore`). Deployed as an Azure Container App.
- **`album-viewer`** — Vue 3 + TypeScript SPA that fetches and displays albums from the API. Built with Vite, tested with Vitest. Deployed as an Azure Container App.

Infrastructure is defined in **`iac/bicep/`** (Azure Container Apps environment, Log Analytics, Application Insights, Azure Blob Storage as Dapr state store).

A **`legacy/albums.cbl`** COBOL file is also present for migration-demo purposes.

---

## Tech Stack

| Layer | Technology |
|---|---|
| Backend | .NET 8, ASP.NET Core Web API, Swagger/OpenAPI, Dapr |
| Frontend | Vue 3, TypeScript, Vite, Vitest, Axios |
| IaC | Azure Bicep |
| Cloud | Azure Container Apps, Azure Blob Storage, Log Analytics, Application Insights |

---

## Architecture

- The API listens on port `3000` (configurable via `ASPNETCORE_URLS`).
- The viewer proxies `/albums` requests to the API via the Vite dev server (`VITE_ALBUM_API_HOST`).
- Dapr HTTP port defaults to `3500` (`DAPR_HTTP_PORT` env var).
- The album collection loaded is controlled by `COLLECTION_ID` (default: `GreatestHits`).

---

## Coding Conventions

### Backend (C#)
- Namespace: `albums_api` (snake_case, matching the project root namespace).
- Controllers live in `albums-api/Controllers/`, models in `albums-api/Models/`.
- Use `record` types for immutable models (see `Album.cs`).
- Return `IActionResult` from controller actions.
- Nullable reference types are **enabled** (`<Nullable>enable</Nullable>`).
- Use `IHttpClientFactory` (injected via DI) for outbound HTTP calls — never instantiate `HttpClient` directly.
- Never concatenate user input into SQL queries — use parameterized queries / `SqlCommand` with parameters.
- Never accept arbitrary file paths from user input — validate and restrict to known directories.

### Frontend (TypeScript / Vue 3)
- Use the Composition API with `<script setup lang="ts">`.
- Define shared types in `src/types/` and export them as interfaces.
- Use `axios` for HTTP calls; handle errors in `try/catch` and surface them via a reactive `error` ref.
- Background color is configurable via `VITE_BACKGROUND_COLOR` env var.

### IaC (Bicep)
- All resource names are derived from a `uniqueSuffix` computed from the subscription ID and resource-group name to ensure global uniqueness.
- Mark secrets (registry password, connection strings) with `@secure()`.
- Co-locate related modules in `iac/bicep/modules/`.

---

## Build & Run

### Backend
```bash
# Build
dotnet build albums-api/albums-api.csproj

# Run (watch mode)
dotnet watch run --project gh-copilot-demo.sln
```

### Frontend
```bash
cd album-viewer
npm install
VITE_ALBUM_API_HOST=localhost:3000 npm run dev
```

### Tests
```bash
# Frontend unit tests
cd album-viewer && npm test

# Backend (if tests are added under albums-api/tests/)
dotnet test
```

---

## Security Guidelines

- The file `albums-api/Controllers/UnsecuredController.cs` contains **intentional vulnerabilities** for demo purposes (SQL injection, path traversal). Do **not** use these patterns in production code.
- Always use parameterized SQL (`SqlCommand` with `SqlParameter`).
- Never pass raw user input to `File.Open` or any file-system API.
- Copilot should flag and fix OWASP Top 10 issues when generating or reviewing code.

---

## Deployment

Deployment targets **Azure Container Apps** using the Bicep templates in `iac/bicep/main.bicep`.  
Required parameters at deploy time: `registryName`, `registryUsername`, `registryPassword` (secret), `apiImage`, `viewerImage`.
