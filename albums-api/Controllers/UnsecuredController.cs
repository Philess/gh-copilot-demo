using Microsoft.Data.SqlClient;
using System.Data;

namespace UnsecureApp.Controllers
{
    public class MyController
    {
        private readonly string fileRoot;
        private readonly string connectionString;

        public MyController(IConfiguration configuration)
        {
            connectionString = configuration.GetConnectionString("Products")
                ?? throw new InvalidOperationException("ConnectionStrings:Products must be configured.");
            string root = configuration["FileAccess:RootDirectory"]
                ?? throw new InvalidOperationException("FileAccess:RootDirectory must be configured.");
            if (!Path.IsPathFullyQualified(root))
            {
                throw new InvalidOperationException("FileAccess:RootDirectory must be an absolute path.");
            }

            fileRoot = Path.GetFullPath(root);
        }

        public string ReadFile(string userInput)
        {
            ArgumentException.ThrowIfNullOrWhiteSpace(userInput);
            if (userInput == "." || userInput == ".." ||
                userInput.IndexOfAny(new[] { '/', '\\', ':' }) >= 0 ||
                userInput.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                throw new ArgumentException("Only a plain filename is allowed.", nameof(userInput));
            }

            for (DirectoryInfo? directory = new DirectoryInfo(fileRoot);
                 directory != null; directory = directory.Parent)
            {
                if (!directory.Exists ||
                    (directory.Attributes & FileAttributes.ReparsePoint) != 0)
                {
                    throw new UnauthorizedAccessException("The file directory must exist and cannot contain symbolic links.");
                }
            }

            string path = Path.Combine(fileRoot, userInput);
            if ((File.GetAttributes(path) & (FileAttributes.ReparsePoint | FileAttributes.Directory)) != 0)
            {
                throw new UnauthorizedAccessException("Only regular, non-linked files are allowed.");
            }

            return File.ReadAllText(path);
        }

        public int GetProduct(string productName)
        {
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                ArgumentNullException.ThrowIfNull(productName);
                using SqlCommand sqlCommand = connection.CreateCommand();
                sqlCommand.CommandText = "SELECT ProductId FROM Products WHERE ProductName = @productName";
                sqlCommand.CommandType = CommandType.Text;
                sqlCommand.Parameters.Add("@productName", SqlDbType.NVarChar, -1).Value = productName;

                connection.Open();
                using SqlDataReader reader = sqlCommand.ExecuteReader();
                if (!reader.Read())
                {
                    throw new KeyNotFoundException("The requested product was not found.");
                }

                return reader.GetInt32(0);
            }
        }

        public void GetObject()
        {
            try
            {
                object o = null;
                o.ToString();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }
        
        }

    }
}