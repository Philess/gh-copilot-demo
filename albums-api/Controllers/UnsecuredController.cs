using Microsoft.Data.SqlClient;
using System.Data;
using System.Text;
using Microsoft.AspNetCore.Mvc;
using System.IO;

namespace UnsecureApp.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class MyController : ControllerBase
    {
        private readonly string connectionString;
        private readonly string allowedDirectory = "/app/data"; // Example safe directory

        public MyController()
        {
            // Ideally, use configuration or dependency injection for connection strings
            connectionString = "YOUR_CONNECTION_STRING_HERE";
        }

        // Secure file read: restricts to allowed directory and validates filename
        [HttpGet("read-file")]
        public ActionResult<string> ReadFile([FromQuery] string fileName)
        {
            if (string.IsNullOrWhiteSpace(fileName) || fileName.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                return BadRequest("Invalid file name.");
            }

            var safePath = Path.Combine(allowedDirectory, fileName);
            var fullPath = Path.GetFullPath(safePath);
            if (!fullPath.StartsWith(allowedDirectory))
            {
                return BadRequest("Access denied.");
            }

            if (!System.IO.File.Exists(fullPath))
            {
                return NotFound("File not found.");
            }

            try
            {
                return System.IO.File.ReadAllText(fullPath, Encoding.UTF8);
            }
            catch (Exception)
            {
                // Log exception securely
                return StatusCode(500, "Error reading file.");
            }
        }

        // Secure SQL: uses parameterized query
        [HttpGet("get-product")]
        public ActionResult<int> GetProduct([FromQuery] string productName)
        {
            if (string.IsNullOrWhiteSpace(productName))
            {
                return BadRequest("Product name required.");
            }

            using (var connection = new SqlConnection(connectionString))
            using (var sqlCommand = new SqlCommand("SELECT ProductId FROM Products WHERE ProductName = @productName", connection))
            {
                sqlCommand.CommandType = CommandType.Text;
                sqlCommand.Parameters.AddWithValue("@productName", productName);
                connection.Open();
                using (var reader = sqlCommand.ExecuteReader())
                {
                    if (reader.Read())
                    {
                        return reader.GetInt32(0);
                    }
                    else
                    {
                        return NotFound("Product not found.");
                    }
                }
            }
        }

        // Improved exception handling example
        [HttpGet("get-object")]
        public IActionResult GetObject()
        {
            try
            {
                object o = null;
                o.ToString();
                return Ok();
            }
            catch (NullReferenceException)
            {
                // Log securely
                return StatusCode(500, "A null reference occurred.");
            }
            catch (Exception)
            {
                // Log securely
                return StatusCode(500, "An error occurred.");
            }
        }
    }
}