using Microsoft.Data.SqlClient;
using System.Data;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;
using System.Threading.Tasks;

namespace UnsecureApp.Controllers
{
    public class MyController
    {
        public string ReadFile(string userInput)
        {
            using (FileStream fs = File.Open(userInput, FileMode.Open))
            {
                return ReadStreamContent(fs);
            }

            return null;
        }

        public async Task<string> ReadFileAsync(string userInput)
        {
            using (FileStream fs = new FileStream(userInput, FileMode.Open, FileAccess.Read, FileShare.Read, 4096, useAsync: true))
            {
                return await ReadStreamContentAsync(fs).ConfigureAwait(false);
            }
        }

        public int GetProduct(string productName)
        {
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                connection.Open();

                using (SqlCommand sqlCommand = CreateProductLookupCommand(connection, productName))
                using (SqlDataReader reader = sqlCommand.ExecuteReader())
                {
                    return reader.GetInt32(0);
                }
            }
        }

        public async Task<int> GetProductAsync(string productName)
        {
            await using SqlConnection connection = new SqlConnection(connectionString);
            await connection.OpenAsync().ConfigureAwait(false);

            await using SqlCommand sqlCommand = CreateProductLookupCommand(connection, productName);
            await using SqlDataReader reader = await sqlCommand.ExecuteReaderAsync(CommandBehavior.SingleRow).ConfigureAwait(false);

            if (await reader.ReadAsync().ConfigureAwait(false))
            {
                return reader.GetInt32(0);
            }

            throw new InvalidOperationException($"Product '{productName}' was not found.");
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
                LogException(e);
            }
        
        }

        public async Task GetObjectAsync()
        {
            try
            {
                object o = null;
                o.ToString();
            }
            catch (Exception e)
            {
                await LogExceptionAsync(e).ConfigureAwait(false);
            }
        }

        private string ReadStreamContent(FileStream fs)
        {
            byte[] b = new byte[1024];
            UTF8Encoding temp = new UTF8Encoding(true);

            while (fs.Read(b, 0, b.Length) > 0)
            {
                return temp.GetString(b);
            }

            return null;
        }

        private async Task<string> ReadStreamContentAsync(FileStream fs)
        {
            byte[] b = new byte[1024];
            UTF8Encoding temp = new UTF8Encoding(true);

            while (await fs.ReadAsync(b, 0, b.Length).ConfigureAwait(false) > 0)
            {
                return temp.GetString(b);
            }

            return null;
        }

        private SqlCommand CreateProductLookupCommand(SqlConnection connection, string productName)
        {
            SqlCommand sqlCommand = connection.CreateCommand();
            sqlCommand.CommandText = "SELECT ProductId FROM Products WHERE ProductName = @ProductName";
            sqlCommand.CommandType = CommandType.Text;
            sqlCommand.Parameters.AddWithValue("@ProductName", productName);
            return sqlCommand;
        }

        private Task LogExceptionAsync(Exception exception)
        {
            LogException(exception);
            return Task.CompletedTask;
        }

        private void LogException(Exception exception)
        {
            Console.WriteLine(exception.ToString());
        }

        private string connectionString = "";
    }
}