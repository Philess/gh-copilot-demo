using albums_api.Controllers;
using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

namespace albums_api.Tests
{
    public class AlbumControllerTests
    {
        private readonly AlbumController _controller = new();

        [Fact]
        public void Get_ReturnsOkWithAllAlbums()
        {
            var result = _controller.Get();

            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsAssignableFrom<List<Album>>(okResult.Value);
            Assert.NotEmpty(albums);
        }

        [Fact]
        public void GetById_ReturnsOkForExistingAlbum()
        {
            var result = _controller.Get(1);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var album = Assert.IsType<Album>(okResult.Value);
            Assert.Equal(1, album.Id);
        }

        [Fact]
        public void GetById_ReturnsNotFoundForMissingAlbum()
        {
            var result = _controller.Get(int.MaxValue);

            Assert.IsType<NotFoundResult>(result);
        }

        [Fact]
        public void GetSorted_ReturnsBadRequestForInvalidSortBy()
        {
            var result = _controller.GetSorted("invalid");

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public void GetSorted_SortsByTitleAscending()
        {
            var result = _controller.GetSorted("title");

            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsAssignableFrom<List<Album>>(okResult.Value);
            var titles = albums.Select(a => a.Title).ToList();
            Assert.Equal(titles.OrderBy(t => t).ToList(), titles);
        }

        [Fact]
        public void GetSorted_SortsByArtistNameAscending()
        {
            var result = _controller.GetSorted("artist");

            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsAssignableFrom<List<Album>>(okResult.Value);
            var artistNames = albums.Select(a => a.Artist.Name).ToList();
            Assert.Equal(artistNames.OrderBy(n => n).ToList(), artistNames);
        }

        [Fact]
        public void GetSorted_SortsByPriceAscending()
        {
            var result = _controller.GetSorted("price");

            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsAssignableFrom<List<Album>>(okResult.Value);
            var prices = albums.Select(a => a.Price).ToList();
            Assert.Equal(prices.OrderBy(p => p).ToList(), prices);
        }

        [Fact]
        public void Search_ReturnsOnlyAlbumsFromRequestedYear()
        {
            var result = _controller.Search(2023);

            var okResult = Assert.IsType<OkObjectResult>(result);
            var albums = Assert.IsAssignableFrom<List<Album>>(okResult.Value);
            Assert.NotEmpty(albums);
            Assert.All(albums, a => Assert.Equal(2023, a.year));
        }

        [Fact]
        public void Create_ReturnsCreatedAtActionWithNewAlbum()
        {
            var artist = new Artist("Controller Test Artist", new DateTime(1990, 1, 1), "Test Town");
            var album = new Album(0, "Controller Test Album", artist, 2024, 7.99, "http://example.com/controller.png");

            var result = _controller.Create(album);

            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var createdAlbum = Assert.IsType<Album>(createdResult.Value);
            Assert.True(createdAlbum.Id > 0);
            Assert.Equal("Controller Test Album", createdAlbum.Title);

            // Cleanup so this test doesn't leave state behind for other tests.
            _controller.Delete(createdAlbum.Id);
        }

        [Fact]
        public void Create_ReturnsBadRequestForNullAlbum()
        {
            var result = _controller.Create(null!);

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public void Update_ReturnsOkForExistingAlbum()
        {
            var artist = new Artist("Update Test Artist", new DateTime(1991, 2, 2), "Update Town");
            var created = Assert.IsType<CreatedAtActionResult>(
                _controller.Create(new Album(0, "Update Test Album", artist, 2024, 8.99, "http://example.com/update.png")));
            var createdAlbum = Assert.IsType<Album>(created.Value);

            var updatedArtist = new Artist("Updated Test Artist", new DateTime(1992, 3, 3), "Updated Town");
            var result = _controller.Update(createdAlbum.Id, new Album(0, "Updated Test Album", updatedArtist, 2025, 9.99, "http://example.com/updated.png"));

            var okResult = Assert.IsType<OkObjectResult>(result);
            var updatedAlbum = Assert.IsType<Album>(okResult.Value);
            Assert.Equal(createdAlbum.Id, updatedAlbum.Id);
            Assert.Equal("Updated Test Album", updatedAlbum.Title);

            _controller.Delete(createdAlbum.Id);
        }

        [Fact]
        public void Update_ReturnsNotFoundForMissingAlbum()
        {
            var artist = new Artist("Nobody", new DateTime(2000, 1, 1), "Nowhere");
            var result = _controller.Update(int.MaxValue, new Album(0, "Does Not Exist", artist, 2000, 0.99, "http://example.com/none.png"));

            Assert.IsType<NotFoundResult>(result);
        }

        [Fact]
        public void Update_ReturnsBadRequestForNullAlbum()
        {
            var result = _controller.Update(1, null!);

            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public void Delete_ReturnsNoContentForExistingAlbum()
        {
            var artist = new Artist("Delete Test Artist", new DateTime(1993, 4, 4), "Delete Town");
            var created = Assert.IsType<CreatedAtActionResult>(
                _controller.Create(new Album(0, "Delete Test Album", artist, 2024, 6.99, "http://example.com/delete.png")));
            var createdAlbum = Assert.IsType<Album>(created.Value);

            var result = _controller.Delete(createdAlbum.Id);

            Assert.IsType<NoContentResult>(result);
            Assert.IsType<NotFoundResult>(_controller.Get(createdAlbum.Id));
        }

        [Fact]
        public void Delete_ReturnsNotFoundForMissingAlbum()
        {
            var result = _controller.Delete(int.MaxValue);

            Assert.IsType<NotFoundResult>(result);
        }
    }
}
