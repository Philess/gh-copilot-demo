using Microsoft.Data.SqlClient;
using System.Data;
using System.Globalization;
using System.Text;

namespace UnsecureApp.Controllers
{
    public class MyController
    {
        private readonly Func<string, FileStream> _fileStreamFactory;
        private readonly string _safeBaseDirectory;
        private readonly string _connectionString;

        public MyController()
            : this(
                path => File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read),
                Directory.GetCurrentDirectory(),
                string.Empty)
        {
        }

        public MyController(Func<string, FileStream> fileStreamFactory, string safeBaseDirectory, string connectionString)
        {
            _fileStreamFactory = fileStreamFactory ?? throw new ArgumentNullException(nameof(fileStreamFactory));
            if (string.IsNullOrWhiteSpace(safeBaseDirectory))
            {
                throw new ArgumentException("A safe base directory must be provided.", nameof(safeBaseDirectory));
            }

            _safeBaseDirectory = Path.GetFullPath(safeBaseDirectory);
            _connectionString = connectionString ?? string.Empty;
        }

        public string ReadFile(string userInput)
        {
            if (string.IsNullOrWhiteSpace(userInput))
            {
                throw new ArgumentException("A relative file path must be provided.", nameof(userInput));
            }

            var normalizedBaseDirectory = EnsureDirectorySeparator(_safeBaseDirectory);
            var candidatePath = Path.GetFullPath(Path.Combine(_safeBaseDirectory, userInput));

            if (!candidatePath.StartsWith(normalizedBaseDirectory, StringComparison.Ordinal))
            {
                throw new UnauthorizedAccessException("Access to the requested file path is not allowed.");
            }

            using var fs = _fileStreamFactory(candidatePath);
            using var reader = new StreamReader(fs, Encoding.UTF8, detectEncodingFromByteOrderMarks: true);

            return reader.ReadToEnd();
        }

        public int GetProduct(string productName)
        {
            if (string.IsNullOrWhiteSpace(productName))
            {
                throw new ArgumentException("Product name is required.", nameof(productName));
            }

            if (string.IsNullOrWhiteSpace(_connectionString))
            {
                throw new InvalidOperationException("A valid database connection string is required.");
            }

            using var connection = new SqlConnection(_connectionString);
            using var sqlCommand = new SqlCommand(
                "SELECT TOP (1) ProductId FROM Products WHERE ProductName = @productName",
                connection)
            {
                CommandType = CommandType.Text
            };

            sqlCommand.Parameters.Add("@productName", SqlDbType.NVarChar, 255).Value = productName;

            connection.Open();
            var result = sqlCommand.ExecuteScalar();

            if (result == null || result == DBNull.Value)
            {
                throw new KeyNotFoundException($"No product found with name '{productName}'.");
            }

            return Convert.ToInt32(result, CultureInfo.InvariantCulture);
        }

        public void GetObject()
        {
            var o = new object();
            _ = o.ToString();
        }

        private static string EnsureDirectorySeparator(string path)
        {
            if (path.EndsWith(Path.DirectorySeparatorChar) || path.EndsWith(Path.AltDirectorySeparatorChar))
            {
                return path;
            }

            return path + Path.DirectorySeparatorChar;
        }
    }
}