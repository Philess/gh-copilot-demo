using System;
using System.IO;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Moq;
using UnsecureApp.Controllers;

namespace UnsecureApp.Tests.Controllers;

[TestClass]
public class MyControllerTests
{
    [TestMethod]
    public void ReadFile_WithMockedFileStream_ReturnsExpectedContent()
    {
        var expectedContent = "Testinhalt";
        var tempDirectory = Path.GetTempPath();
        var tempPath = Path.Combine(tempDirectory, $"mocked-file-{Guid.NewGuid():N}.txt");
        try
        {
            File.WriteAllText(tempPath, expectedContent, Encoding.UTF8);
            var mockFileStream = new Mock<FileStream>(tempPath, FileMode.Open, FileAccess.Read, FileShare.Read)
            {
                CallBase = true
            };

            var fileName = Path.GetFileName(tempPath);
            var controller = new MyController(_ => mockFileStream.Object, tempDirectory, string.Empty);

            var result = controller.ReadFile(fileName);

            Assert.IsNotNull(result);
            Assert.AreEqual(expectedContent, result);
        }
        finally
        {
            if (File.Exists(tempPath))
            {
                File.Delete(tempPath);
            }
        }
    }

    [TestMethod]
    [ExpectedException(typeof(FileNotFoundException))]
    public void ReadFile_FileNotFound_ThrowsException()
    {
        var controller = new MyController();
        controller.ReadFile("nonexistentfile.txt");
    }

    [TestMethod]
    public void GetObject_DoesNotThrow()
    {
        var controller = new MyController();
        controller.GetObject();
    }
}
