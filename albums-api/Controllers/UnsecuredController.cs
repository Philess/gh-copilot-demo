using Microsoft.Data.SqlClient;
using System.Data;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;
using System.Text.RegularExpressions;
using System.Web;

namespace UnsecureApp.Controllers
{
    public class MyController
    {
        private string connectionString = "";

        public HttpResponse Response { get; set; }

        public string ReadFile(string userInput)
        {
            if(!IsValidInput(userInput)) throw new ArgumentException("Invalid input");

            string filePath = Path.Combine("C:\\uploads", userInput);
            if (!filePath.StartsWith("C:\\uploads"))
            {
                throw new ArgumentException("Invalid file path");
            }

            try
            {
                using (FileStream fs = File.Open(filePath, FileMode.Open))
                {
                    byte[] b = new byte[1024];
                    UTF8Encoding temp = new UTF8Encoding(true);

                    while (fs.Read(b, 0, b.Length) > 0)
                    {
                        string encodedString = HttpUtility.HtmlEncode(temp.GetString(b));
                        return encodedString;
                    }
                }
            }
            catch (Exception e)
            {
                // Log the exception
                return null;
            }

            return null;
        }

        public bool IsValidInput(string userInput)
        {
            // Define a whitelist of allowed characters and patterns
            string allowedPattern = "^[a-zA-Z0-9_]*$";

            // Validate the user input against the whitelist
            if (Regex.IsMatch(userInput, allowedPattern))
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        public int GetProduct(string productName)
        {
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                SqlCommand sqlCommand = new SqlCommand()
                {
                    CommandText = "SELECT ProductId FROM Products WHERE ProductName = @ProductName",
                    CommandType = CommandType.Text,
                    Connection = connection
                };
                sqlCommand.Parameters.AddWithValue("@ProductName", productName);

                connection.Open();
                SqlDataReader reader = sqlCommand.ExecuteReader();
                if (reader.Read())
                {
                    return reader.GetInt32(0);
                }
                else
                {
                    return -1; // Or throw an exception
                }
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
                this.Response.StatusCode = 500;
                this.Response.WriteAsJsonAsync("An error occurred");
            }
        }
    }
}