using Microsoft.Data.SqlClient;
using System.Data;
using System.Text;

namespace albums_api.Controllers
{
    public class MyController
    {

        public string ReadFile(string userInput)
        {
            if (string.IsNullOrWhiteSpace(userInput))
            {
                throw new ArgumentException("Path is required", nameof(userInput));
            }

            string fullPath = Path.GetFullPath(userInput);
            string basePath = Path.GetFullPath(".");
            if (!fullPath.StartsWith(basePath, StringComparison.Ordinal))
            {
                throw new UnauthorizedAccessException("Access denied: path traversal attempt detected");
            }

            using FileStream fs = File.Open(fullPath, FileMode.Open);
            using StreamReader reader = new StreamReader(fs, Encoding.UTF8);
            return reader.ReadToEnd();
        }

        public int GetProduct(string productName)
        {
            if (string.IsNullOrWhiteSpace(productName))
            {
                throw new ArgumentException("Product name is required", nameof(productName));
            }

            if (string.IsNullOrWhiteSpace(connectionString))
            {
                throw new InvalidOperationException("Database connection string is not configured.");
            }

            using SqlConnection connection = new SqlConnection(connectionString);
            using SqlCommand sqlCommand = new SqlCommand()
            {
                Connection = connection,
                CommandText = "SELECT ProductId FROM Products WHERE ProductName = @ProductName",
                CommandType = CommandType.Text,
            };
            sqlCommand.Parameters.AddWithValue("@ProductName", productName);

            connection.Open();
            using SqlDataReader reader = sqlCommand.ExecuteReader();
            if (reader.Read())
            {
                return reader.GetInt32(0);
            }
            throw new InvalidOperationException("Product not found");
        }

        private readonly string connectionString = "";
    }
}