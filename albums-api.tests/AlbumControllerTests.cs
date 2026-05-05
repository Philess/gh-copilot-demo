using albums_api.Controllers;
using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

namespace albums_api.tests;

public class AlbumControllerTests
{
    private readonly AlbumController _controller = new();

    [Fact]
    public void GetAll_ReturnsOkWithAlbums()
    {
        var result = _controller.Get();

        var ok = Assert.IsType<OkObjectResult>(result);
        var albums = Assert.IsAssignableFrom<List<Album>>(ok.Value);
        Assert.NotEmpty(albums);
    }

    [Fact]
    public void GetById_ExistingId_ReturnsOkWithAlbum()
    {
        var result = _controller.Get(1);

        var ok = Assert.IsType<OkObjectResult>(result);
        var album = Assert.IsType<Album>(ok.Value);
        Assert.Equal(1, album.Id);
    }

    [Fact]
    public void GetById_NonExistingId_ReturnsNotFound()
    {
        var result = _controller.Get(9999);

        Assert.IsType<NotFoundResult>(result);
    }

    [Fact]
    public void SearchByYear_MatchingYear_ReturnsAlbums()
    {
        var result = _controller.SearchByYear(2023);

        var ok = Assert.IsType<OkObjectResult>(result);
        var albums = Assert.IsAssignableFrom<List<Album>>(ok.Value);
        Assert.All(albums, a => Assert.Equal(2023, a.Year));
    }

    [Fact]
    public void SearchByYear_NoMatch_ReturnsEmptyList()
    {
        var result = _controller.SearchByYear(1800);

        var ok = Assert.IsType<OkObjectResult>(result);
        var albums = Assert.IsAssignableFrom<List<Album>>(ok.Value);
        Assert.Empty(albums);
    }

    [Fact]
    public void Post_ValidAlbum_ReturnsCreatedWithAlbum()
    {
        var newAlbum = new Album(0, "Test Album", new Artist("Test Artist", DateTime.Now, "Test City"), 9.99, "https://example.com/img.png", 2025);

        var result = _controller.Post(newAlbum);

        var created = Assert.IsType<CreatedAtActionResult>(result);
        var album = Assert.IsType<Album>(created.Value);
        Assert.Equal("Test Album", album.Title);
        Assert.True(album.Id > 0);
    }

    [Fact]
    public void Put_ExistingId_ReturnsOkWithUpdatedAlbum()
    {
        var updated = new Album(0, "Updated Title", new Artist("Artist", DateTime.Now, "City"), 11.99, "https://example.com/img.png", 2024);

        var result = _controller.Put(1, updated);

        var ok = Assert.IsType<OkObjectResult>(result);
        var album = Assert.IsType<Album>(ok.Value);
        Assert.Equal(1, album.Id);
        Assert.Equal("Updated Title", album.Title);
    }

    [Fact]
    public void Put_NonExistingId_ReturnsNotFound()
    {
        var updated = new Album(0, "Title", new Artist("Artist", DateTime.Now, "City"), 9.99, "https://example.com/img.png", 2024);

        var result = _controller.Put(9999, updated);

        Assert.IsType<NotFoundResult>(result);
    }

    [Fact]
    public void Delete_ExistingId_ReturnsNoContent()
    {
        // Create a temporary album to delete so we don't disrupt other tests
        var temp = Album.Create(new Album(0, "Temp", new Artist("Temp", DateTime.Now, "City"), 1.00, "https://example.com/img.png", 2020));

        var result = _controller.Delete(temp.Id);

        Assert.IsType<NoContentResult>(result);
    }

    [Fact]
    public void Delete_NonExistingId_ReturnsNotFound()
    {
        var result = _controller.Delete(9999);

        Assert.IsType<NotFoundResult>(result);
    }
}
