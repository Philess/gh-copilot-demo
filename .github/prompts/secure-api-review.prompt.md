# .NET C# Code Review Checklist

## Security Fundamentals
* Ensure all endpoints are protected by authentication and authorization
* Validate all user inputs and sanitize data
* Implement rate limiting and throttling
* Implement logging and monitoring for security events

## .NET C# Specific Security
* Avoid unsafe code blocks unless absolutely necessary
* Use secure string handling for sensitive information
* Implement proper exception handling with appropriate exception types
* Prevent SQL injection by using parameterized queries or ORM frameworks
* Apply the Principle of Least Privilege in your application design

## Code Quality & Performance
* Follow C# coding conventions and naming standards
* Use asynchronous programming (async/await) for I/O operations
* Implement proper disposal patterns for IDisposable resources
* Avoid LINQ operations that may cause performance issues on large datasets
* Consider thread safety in shared state scenarios

## Architecture & Design
* Follow SOLID principles in your class design
* Use dependency injection for better testability
* Implement appropriate design patterns for the problem domain
* Structure projects and namespaces logically
* Use interfaces for better abstraction and testability

## Documentation & Maintainability
* Include documentation for public APIs
* Keep methods focused and maintainable (avoid excessive complexity)
* Document non-obvious implementation decisions
* Use meaningful variable and method names
* Maintain consistency in code formatting