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

The [`album-viewer`](./album-viewer) is a modern Vue.js 3 application built with TypeScript through which the albums retrieved by the API are surfaced. The application uses the Vue 3 Composition API with full TypeScript support for enhanced developer experience and type safety. In order to display the repository of albums, the album viewer contacts the backend album API.

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

### Album API routes

The API uses the `/albums` base route. Interactive documentation is available at
`http://localhost:3000/swagger` when running in Development.

| Method | Route | Successful response |
|--------|-------|---------------------|
| GET | `/albums` | `200 OK` with all albums |
| GET | `/albums/{id}` | `200 OK` with one album |
| GET | `/albums/sort?sortBy=title` | `200 OK` sorted by `title`, `artist`, or `price` |
| GET | `/albums/search?year=2024` | `200 OK` with albums from exactly that year, or `[]` |
| POST | `/albums` | `201 Created` with the album and a `Location` header |
| PUT | `/albums/{id}` | `200 OK` with the fully replaced album |
| DELETE | `/albums/{id}` | `204 No Content` |

Both POST and PUT require all five fields:

```json
{
  "title": "New Horizons",
  "artist": "The Containers",
  "year": 2024,
  "price": 12.99,
  "image_url": "https://example.com/cover.png"
}
```

The response contains the same fields plus a server-assigned integer `id`. Do
not supply an ID in the request body; PUT retains the ID from the route and
does not create a missing album. Duplicate titles are allowed.

- `title` and `artist` must be nonblank.
- `year` must be an integer between 1 and 9999.
- `price` must be finite and nonnegative; zero is valid.
- `image_url` must be an absolute HTTP or HTTPS URL.
- Missing or invalid write fields and missing/invalid search years return
  `400 Bad Request` with validation Problem Details.
- Unknown IDs return `404 Not Found`, including PUT and repeated DELETE requests.
- Unsupported sort fields return `400 Bad Request`.

The six sample albums are seeded once per application instance. Changes are
shared across requests and visible in list, detail, sort, and search results.
IDs increase and are not reused after deletion. All state resets on restart;
separate application instances do not share data. These demo endpoints do not
add authentication or authorization.

Use [`albums-api.http`](./albums-api/albums-api.http) for runnable requests,
including error cases. The write examples refer to the ID returned by the
named create request.

Run the backend build and isolated integration tests from the repository root:

```bash
dotnet build albums-api/albums-api.csproj
dotnet test albums-api.Tests/albums-api.Tests.csproj
```

The tests start an in-process host with fresh seed data for each test. They do
not need a running API, SQL database, or file/product helper configuration.

### File and product lookup sample configuration

The sample [`MyController`](./albums-api/Controllers/UnsecuredController.cs)
requires `ConnectionStrings:Products` and `FileAccess:RootDirectory` through .NET
configuration. Supply the database connection string through user secrets or a
deployment secret, not committed configuration. Environment variable equivalents
are `ConnectionStrings__Products` and `FileAccess__RootDirectory`.

The file root must be an existing absolute directory. File reads accept only
plain filenames, not paths, and reject symbolic links in both the root path and
the requested file. Keep this directory and its ancestors under trusted
administrative control: untrusted users must not be able to replace files or
directories between validation and opening, or introduce hard links. All files
in the root should be approved for disclosure to the caller; callers must enforce
authorization before invoking this helper. File reads return the complete UTF-8
text and propagate access and I/O errors.

Product lookups use a parameterized SQL query and throw `KeyNotFoundException`
when no matching product exists. Use a least-privileged database account with
only the required read access.

### Alternative: GitHub Codespaces

The easiest way is to open this solution in a GitHub Codespace, or run it locally in a devcontainer. The development environment will be automatically configured for you.