using Xunit;
using albums_api.Controllers;
using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

namespace albums_api.Tests
{
    public class AlbumControllerTests
    {
        private readonly AlbumController _controller;

        public AlbumControllerTests()
        {
            _controller = new AlbumController();
        }

        [Fact]
        public void Get_ReturnsAllAlbums()
        {
            // Act
            var result = _controller.Get(null, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsType<List<Album>>(okResult.Value);
            Assert.Equal(6, albums.Count);
        }

        [Fact]
        public void Get_FiltersByYear()
        {
            // Act
            var result = _controller.Get(null, 2020);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsType<List<Album>>(okResult.Value);
            Assert.Single(albums);
            Assert.Equal(2020, albums[0].Year);
        }

        [Theory]
        [InlineData("title")]
        [InlineData("artist")]
        [InlineData("price")]
        public void Get_SortsByParameter(string sortBy)
        {
            // Act
            var result = _controller.Get(sortBy, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsType<List<Album>>(okResult.Value);
            Assert.NotEmpty(albums);
        }

        [Fact]
        public void GetById_WithValidId_ReturnsAlbum()
        {
            // Act
            var result = _controller.Get(1);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var album = Assert.IsType<Album>(okResult.Value);
            Assert.Equal(1, album.Id);
            Assert.Equal("You, Me and an App Id", album.Title);
        }

        [Fact]
        public void GetById_WithInvalidId_ReturnsNotFound()
        {
            // Act
            var result = _controller.Get(999);

            // Assert
            Assert.IsType<NotFoundResult>(result);
        }

        [Fact]
        public void Post_WithValidAlbum_ReturnsCreatedAtAction()
        {
            // Arrange
            var newAlbum = new Album(7, "Test Album", new Artist("Test Artist", new DateTime(2020, 1, 1), "Test City"), 2025, 9.99, "https://example.com/test.jpg");

            // Act
            var result = _controller.Post(newAlbum);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            Assert.Equal(nameof(AlbumController.Get), createdResult.ActionName);
            Assert.Equal(newAlbum.Id, ((Album)createdResult.Value).Id);
        }

        [Fact]
        public void Post_WithNullAlbum_ReturnsBadRequest()
        {
            // Act
            var result = _controller.Post(null);

            // Assert
            Assert.IsType<BadRequestResult>(result);
        }

        [Fact]
        public void Put_WithValidId_ReturnsOk()
        {
            // Arrange
            var updatedAlbum = new Album(1, "Updated Title", new Artist("Updated Artist", new DateTime(2020, 1, 1), "Updated City"), 2025, 15.99, "https://example.com/updated.jpg");

            // Act
            var result = _controller.Put(1, updatedAlbum);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            Assert.Equal(updatedAlbum, okResult.Value);
        }

        [Fact]
        public void Put_WithInvalidId_ReturnsNotFound()
        {
            // Arrange
            var album = new Album(999, "Test", new Artist("Test", new DateTime(2020, 1, 1), "Test"), 2025, 9.99, "https://example.com/test.jpg");

            // Act
            var result = _controller.Put(999, album);

            // Assert
            Assert.IsType<NotFoundResult>(result);
        }

        [Fact]
        public void Delete_WithValidId_ReturnsNoContent()
        {
            // Act
            var result = _controller.Delete(1);

            // Assert
            Assert.IsType<NoContentResult>(result);
        }

        [Fact]
        public void Delete_WithInvalidId_ReturnsNotFound()
        {
            // Act
            var result = _controller.Delete(999);

            // Assert
            Assert.IsType<NotFoundResult>(result);
        }
    }
}
