using albums_api.Controllers;
using albums_api.Models;
using Microsoft.AspNetCore.Mvc;
using Xunit;

namespace albums_api.Tests.Controllers;

public class AlbumControllerTests
{
    [Fact]
    public void Get_WithExistingId_ReturnsAlbum()
    {
        var controller = new AlbumController();

        var result = controller.Get(1);

        var okResult = Assert.IsType<OkObjectResult>(result);
        var album = Assert.IsType<Album>(okResult.Value);
        Assert.Equal(1, album.Id);
    }

    [Fact]
    public void Get_WithMissingId_ReturnsNotFound()
    {
        var controller = new AlbumController();

        var result = controller.Get(int.MaxValue);

        Assert.IsType<NotFoundResult>(result);
    }
}