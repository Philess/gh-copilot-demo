# Github Copilot demo 

## Demo Scenarios

### To start discovering Github Copilot jump to [`The Ultimate GitHub Copilot Tutorial on MOAW`](https://aka.ms/github-copilot-hol)
<br/>


## Solution Overview


This repository has been inspired by the [Azure Container Apps: Dapr Albums Sample](https://github.com/Azure-Samples/containerapps-dapralbums)

It's used as a code base to demonstrate Github Copilot capabilities.

The solution is composed of two services: the .net album API and the NodeJS album viewer.


### Album API (`album-api`)

The [`album-api`](./album-api) is an .NET 8 minimal Web API that manage a list of Albums in memory.

### Album Viewer (`album-viewer`)

The [`album-viewer`](./album-viewer) is a modern Vue.js 3 application built with TypeScript through which the albums retrieved by the API are surfaced. The application uses the Vue 3 Composition A[...]

## Getting Started

There are multiple ways to run this solution locally. Choose the method that best fits your development workflow.

### Prerequisites

- [.NET 8 SDK](https://dotnet.microsoft.com/download/dotnet/8.0)
- [Node.js](https://nodejs.org/) (version 16 or higher)
- [TypeScript](https://www.typescriptlang.org/) (automatically installed with project dependencies)
- [Visual Studio Code](https://code.visualstudio.com/) (recommended)

### Option 1: Using VS Code Debug Panel (Recommended)

This is the easiest way to run the solution with full debugging capabilities.

1. Open the solution in Visual Studio Code
2. Open the Debug panel (Ctrl+Shift+D / Cmd+Shift+D)
3. Select **"All services"** from the dropdown
4. Click the green play button or press F5

This will automatically:
- Build the .NET API and start it on `http://localhost:3000`
- Start the Vue.js TypeScript app on `http://localhost:3001`
- Open both services in your default browser

You can also run individual services:
- **"C#: Album API Debug"** - Runs only the .NET API
- **"Node.js: Album Viewer Debug"** - Runs only the Vue.js TypeScript frontend

### Option 2: Command Line

#### Starting the Album API (.NET)

```powershell
# Navigate to the API directory
cd albums-api

# Restore dependencies (first time only)
dotnet restore

# Run the API
dotnet run
```

The API will start on `http://localhost:3000` and you can access the Swagger documentation at `http://localhost:3000/swagger`.

#### Starting the Album Viewer (Vue.js + TypeScript)

```powershell
# Navigate to the viewer directory
cd album-viewer

# Install dependencies (first time only)
npm install

# Start the development server
npm run dev

# Optional: Run TypeScript type checking
npm run type-check
```

The Vue.js TypeScript app will start on `http://localhost:3001` and automatically open in your browser.

#### Running Both Services

You can run both services simultaneously using separate terminal windows:

```powershell
# Terminal 1 - Start the API
cd albums-api
dotnet run

# Terminal 2 - Start the Vue TypeScript app
cd album-viewer
npm run dev
```

### Environment Configuration

The solution uses the following default configuration:

- **Album API**: Runs on `http://localhost:3000`
- **Album Viewer**: Runs on `http://localhost:3001` (TypeScript + Vue 3)
- **API Endpoint**: The Vue app is configured to call the API at `localhost:3000`

If you need to change these settings, you can modify:
- API port: `albums-api/Properties/launchSettings.json`
- Vue app configuration: Environment variables in `.vscode/launch.json` or set `VITE_ALBUM_API_HOST` environment variable

### Alternative: GitHub Codespaces

The easiest way is to open this solution in a GitHub Codespace, or run it locally in a devcontainer. The development environment will be automatically configured for you.

---

## Deploying to Azure

This repository can be deployed to Azure. The steps below show a simple approach using Azure Container Registry (ACR) and Azure Container Apps to host both the API and the viewer. Adjust the resource names, locations, and image tags to suit your environment.

Prerequisites:
- Azure CLI installed and signed-in (`az login`)
- Docker installed (or use `az acr build`)

1) Create a resource group and an Azure Container Registry

```bash
az login
az group create --name myResourceGroup --location westeurope
az acr create --resource-group myResourceGroup --name myAcrName --sku Basic --admin-enabled true
ACR_LOGIN_SERVER=$(az acr show -n myAcrName -g myResourceGroup --query loginServer -o tsv)
```

2) Build and push container images (example using Docker)

```bash
# Build and push albums-api
docker build -t $ACR_LOGIN_SERVER/albums-api:1.0 ./albums-api
docker push $ACR_LOGIN_SERVER/albums-api:1.0

# Build and push album-viewer
docker build -t $ACR_LOGIN_SERVER/album-viewer:1.0 ./album-viewer
docker push $ACR_LOGIN_SERVER/album-viewer:1.0
```

Alternatively use ACR Tasks to build without a local Docker daemon:

```bash
az acr build --registry myAcrName --image albums-api:1.0 ./albums-api
az acr build --registry myAcrName --image album-viewer:1.0 ./album-viewer
```

3) Create a Container Apps environment

```bash
az provider register --namespace Microsoft.Web
az containerapp env create --name myEnv --resource-group myResourceGroup --location westeurope
```

4) Deploy the API to Container Apps

```bash
az containerapp create \
  --name albums-api \
  --resource-group myResourceGroup \
  --environment myEnv \
  --image $ACR_LOGIN_SERVER/albums-api:1.0 \
  --ingress 'external' \
  --target-port 3000
```

5) Deploy the Viewer to Container Apps and configure it to call the API

First get the API URL (FQDN) from the previous step or via:

```bash
API_URL=$(az containerapp show --name albums-api --resource-group myResourceGroup --query properties.configuration.ingress.fqdn -o tsv)
```

Create the viewer container app and set an environment variable so the frontend can reach the API:

```bash
az containerapp create \
  --name album-viewer \
  --resource-group myResourceGroup \
  --environment myEnv \
  --image $ACR_LOGIN_SERVER/album-viewer:1.0 \
  --ingress 'external' \
  --target-port 3001 \
  --env-vars VITE_ALBUM_API_HOST=https://$API_URL
```

6) Configure CORS and secrets

- If you use the permissive CORS policy in development, update the API to restrict allowed origins before production deployment.
- Use Container Apps secrets or Azure Key Vault to store any sensitive configuration and reference them in the container app.

7) Verify

- Open the viewer FQDN in your browser (use `az containerapp show` to get its FQDN) and confirm the frontend can call the API and display albums.

Notes and alternatives:
- You can host the Vue frontend in Azure Static Web Apps for a simpler, cheaper option; then use the API in Container Apps or App Service.
- For production, consider configuring TLS, scaling rules, health probes, and a VNet integration if needed.

---

If you want, I can add:
- A GitHub Actions workflow to build and push images to ACR and deploy to Container Apps, or
- An example `az cli` script that parameterizes resource names and tags.
