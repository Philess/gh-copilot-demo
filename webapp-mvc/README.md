# WebApp MVC - ASP.NET Core 9.0

A modern ASP.NET Core 9.0 MVC web application with three main views.

## Features

- **Home Page**: Welcome page with introduction to the application
- **Users Page**: User management interface with a sample user table
- **Products Page**: Product catalog with card-based layout

## Project Structure

```
webapp-mvc/
├── Controllers/
│   ├── HomeController.cs
│   ├── UsersController.cs
│   └── ProductsController.cs
├── Views/
│   ├── Home/
│   │   └── Index.cshtml
│   ├── Users/
│   │   └── Index.cshtml
│   ├── Products/
│   │   └── Index.cshtml
│   ├── Shared/
│   │   ├── _Layout.cshtml
│   │   └── Error.cshtml
│   ├── _ViewStart.cshtml
│   └── _ViewImports.cshtml
├── wwwroot/
│   ├── css/
│   │   └── site.css
│   └── js/
│       └── site.js
├── Properties/
│   └── launchSettings.json
├── Program.cs
├── webapp-mvc.csproj
├── appsettings.json
└── appsettings.Development.json
```

## Getting Started

### Prerequisites

- .NET 9.0 SDK

### Running the Application

1. Navigate to the project directory:
   ```bash
   cd webapp-mvc
   ```

2. Restore dependencies:
   ```bash
   dotnet restore
   ```

3. Run the application:
   ```bash
   dotnet run
   ```

4. Open your browser and navigate to:
   - HTTP: http://localhost:5000
   - HTTPS: https://localhost:5001

## Technology Stack

- ASP.NET Core 9.0
- MVC Pattern
- Razor Views
- C# 13

## License

This project is open source and available for educational purposes.
