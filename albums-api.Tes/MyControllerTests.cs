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
        var expectedBytes = Encoding.UTF8.GetBytes(expectedContent);

        var tempPath = Path.GetTempFileName();
        try
        {
            using var backingStream = new FileStream(tempPath, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite);
            var mockFileStream = new Mock<FileStream>(backingStream.SafeFileHandle, FileAccess.ReadWrite);

            var alreadyRead = false;
            mockFileStream
                .Setup(fs => fs.Read(It.IsAny<byte[]>(), 0, It.IsAny<int>()))
                .Returns((byte[] buffer, int offset, int count) =>
                {
                    if (alreadyRead)
                    {
                        return 0;
                    }

                    alreadyRead = true;
                    Array.Copy(expectedBytes, 0, buffer, offset, expectedBytes.Length);
                    return expectedBytes.Length;
                });

            var controller = new MyController(_ => mockFileStream.Object, Path.GetTempPath(), string.Empty);

            var result = controller.ReadFile("irrelevant.txt");

            Assert.IsNotNull(result);
            Assert.IsTrue(result.Contains(expectedContent));
        }
        finally
        {
            File.Delete(tempPath);
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
