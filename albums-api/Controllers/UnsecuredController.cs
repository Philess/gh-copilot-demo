using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Data.SqlClient;
using System.Data;
using System.Text;

namespace UnsecureApp.Controllers
{
    [Authorize]
    [ApiController]
    [Route("api/secure")]
    public class MyController : ControllerBase
    {
        private readonly string? connectionString;
        private readonly string allowedFileDirectory;

        public MyController(IConfiguration configuration, IWebHostEnvironment environment)
        {
            connectionString = configuration.GetConnectionString("DefaultConnection");

            string configuredDirectory = configuration["AllowedFileDirectory"]
                ?? Path.Combine(environment.ContentRootPath, "Data");
            allowedFileDirectory = Path.GetFullPath(configuredDirectory);
        }

        [HttpGet("files/{fileName}")]
        public ActionResult<string> ReadFile(string fileName)
        {
            if (string.IsNullOrWhiteSpace(fileName))
            {
                return BadRequest("A file name is required.");
            }

            string requestedPath = Path.GetFullPath(Path.Combine(allowedFileDirectory, fileName));
            string directoryPrefix = allowedFileDirectory.TrimEnd(Path.DirectorySeparatorChar)
                + Path.DirectorySeparatorChar;

            if (!requestedPath.StartsWith(directoryPrefix, StringComparison.OrdinalIgnoreCase))
            {
                return BadRequest("The requested file path is invalid.");
            }

            if (!System.IO.File.Exists(requestedPath))
            {
                return NotFound();
            }

            return Ok(System.IO.File.ReadAllText(requestedPath, new UTF8Encoding(true)));
        }

        [HttpGet("products/{productName}")]
        public ActionResult<int> GetProduct(string productName)
        {
            if (string.IsNullOrWhiteSpace(productName))
            {
                return BadRequest("A product name is required.");
            }

            if (string.IsNullOrWhiteSpace(connectionString))
            {
                return Problem("The product data source is unavailable.", statusCode: StatusCodes.Status503ServiceUnavailable);
            }

            using SqlConnection connection = new(connectionString);
            using SqlCommand sqlCommand = new(
                "SELECT ProductId FROM Products WHERE ProductName = @productName",
                connection);
            sqlCommand.CommandType = CommandType.Text;
            sqlCommand.Parameters.Add("@productName", SqlDbType.NVarChar, 200).Value = productName;

            connection.Open();
            object? productId = sqlCommand.ExecuteScalar();
            return productId is null or DBNull ? NotFound() : Ok(Convert.ToInt32(productId));
        }
    }
}