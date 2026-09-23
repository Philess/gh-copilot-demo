using Microsoft.Data.SqlClient;
using System.Data;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;

namespace UnsecureApp.Controllers
{
    public class MyController
    {

        public string ReadFile(string userInput)
        {
            using (FileStream fs = File.Open(userInput, FileMode.Open))
            {
                return ReadFirstBuffer(fs);
            }
        }

        public int GetProduct(string productName)
        {
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                SqlCommand sqlCommand = CreateProductCommand(productName);

                SqlDataReader reader = sqlCommand.ExecuteReader();
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
                LogException(e);
            }
        
        }

        private static string ReadFirstBuffer(FileStream fileStream)
        {
            byte[] buffer = new byte[1024];
            UTF8Encoding encoding = new UTF8Encoding(true);

            while (fileStream.Read(buffer, 0, buffer.Length) > 0)
            {
                return encoding.GetString(buffer);
            }

            return null;
        }

        private static SqlCommand CreateProductCommand(string productName)
        {
            return new SqlCommand()
            {
                CommandText = "SELECT ProductId FROM Products WHERE ProductName = '" + productName + "'",
                CommandType = CommandType.Text,
            };
        }

        private static void LogException(Exception exception)
        {
            Console.WriteLine(exception.ToString());
        }

        private string connectionString = "";
    }
}