# ASP.NET Core 9.0 Project

This is an ASP.NET Core 9.0 web application that includes three main views: Index, Users, and Products. 

## Project Structure

The project is organized into the following directories:

- **Controllers**: Contains the controllers that handle user requests and return views.
  - `HomeController.cs`: Manages the home page.
  - `UsersController.cs`: Manages user-related actions.
  - `ProductsController.cs`: Manages product-related actions.

- **Models**: Contains the data models used in the application.
  - `User.cs`: Defines the User model with properties such as Id, Name, and Email.
  - `Product.cs`: Defines the Product model with properties such as Id, Name, and Price.

- **Views**: Contains the Razor views for the application.
  - **Home**: Contains the home page view.
    - `Index.cshtml`: The Razor view for the home page.
  - **Users**: Contains the users page view.
    - `Index.cshtml`: The Razor view for the users page.
  - **Products**: Contains the products page view.
    - `Index.cshtml`: The Razor view for the products page.

- **wwwroot**: Contains static files such as CSS and JavaScript.
  - **css**: Contains the CSS styles for the application.
    - `site.css`: The main stylesheet.
  - **js**: Contains JavaScript files for the application.
    - `site.js`: The main JavaScript file.

- **Properties**: Contains project properties.
  - `launchSettings.json`: Contains settings for launching the application.

- **Configuration Files**:
  - `appsettings.json`: Contains configuration settings for the application.
  - `Program.cs`: The entry point of the application.
  - `aspnetcore-9-app.csproj`: The project file for the ASP.NET Core application.

## Getting Started

To run the application, ensure you have the .NET SDK installed. You can then build and run the application using the following commands:

```bash
dotnet build
dotnet run
```

Visit `http://localhost:5000` in your browser to view the application.