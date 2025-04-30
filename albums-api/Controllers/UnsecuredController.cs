using Microsoft.Data.SqlClient;
using System.Data;
using System.Text;
using Microsoft.Extensions.Logging;

namespace UnsecureApp.Controllers
{
    public class DataAccessException : Exception
    {
        public DataAccessException(string message, Exception innerException)
            : base(message, innerException) { }
    }

    public class MyController
    {
        private readonly string _connectionString;
        private readonly ILogger<MyController> _logger;

        public MyController(string connectionString, ILogger<MyController> logger)
        {
            _connectionString = connectionString ?? throw new ArgumentNullException(nameof(connectionString));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        // Removed insecure ReadFile method

        public async Task<int?> GetProductAsync(string productName)
        {
            if (string.IsNullOrWhiteSpace(productName))
                throw new ArgumentException("Product name must not be empty.", nameof(productName));

            try
            {
                using (SqlConnection connection = new SqlConnection(_connectionString))
                {
                    await connection.OpenAsync();
                    using (SqlCommand sqlCommand = new SqlCommand(
                        "SELECT ProductId FROM Products WHERE ProductName = @ProductName", connection))
                    {
                        sqlCommand.CommandType = CommandType.Text;
                        sqlCommand.Parameters.AddWithValue("@ProductName", productName);

                        using (SqlDataReader reader = await sqlCommand.ExecuteReaderAsync())
                        {
                            if (await reader.ReadAsync())
                            {
                                return reader.GetInt32(0);
                            }
                        }
                    }
                }
            }
            catch (SqlException ex)
            {
                _logger.LogError(ex, "Database error occurred while retrieving product.");
                throw new DataAccessException("An error occurred while accessing the database.", ex);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Unexpected error occurred.");
                throw;
            }
            return null;
        }

        public string GetObjectToString(object o)
        {
            if (o == null)
                throw new ArgumentNullException(nameof(o), "Object cannot be null.");
            return o.ToString();
        }
    }
}