using albums_api.Controllers;
using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

namespace albums_api.Tests;

[TestClass]
public class AlbumControllerTests
{
    private AlbumController _controller = null!;

    [TestInitialize]
    public void Setup()
    {
        // Reset in-memory store before each test via reflection to ensure isolation
        var field = typeof(Album).GetField("_albums", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)!;
        var idField = typeof(Album).GetField("_nextId", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)!;
        field.SetValue(null, new List<Album>
        {
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateOnly(1992, 5, 14), "Seattle"), 2020, 10.99, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateOnly(1988, 10, 21), "Austin"), 2021, 13.99, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateOnly(1990, 2, 7), "Dublin"), 2022, 13.99, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateOnly(1985, 7, 2), "Amsterdam"), 2020, 12.99, "https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateOnly(1991, 11, 30), "Berlin"), 2021, 12.99, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateOnly(1987, 3, 19), "Chicago"), 2022, 14.99, "https://aka.ms/albums-containerappslogo")
        });
        idField.SetValue(null, 7);
        _controller = new AlbumController();
    }

    // --- GET all ---

    [TestMethod]
    public void GetAll_Returns200WithAlbums()
    {
        var result = _controller.Get() as OkObjectResult;
        Assert.IsNotNull(result);
        Assert.AreEqual(200, result.StatusCode);
    }

    // --- GET by id ---

    [TestMethod]
    public void GetById_ExistingId_Returns200()
    {
        var result = _controller.Get(1) as OkObjectResult;
        Assert.IsNotNull(result);
        Assert.AreEqual(200, result.StatusCode);
    }

    [TestMethod]
    public void GetById_UnknownId_Returns404()
    {
        var result = _controller.Get(999);
        Assert.IsInstanceOfType(result, typeof(NotFoundResult));
    }

    // --- Search by year ---

    [TestMethod]
    public void Search_ReturnsOnlyMatchingYear()
    {
        var result = _controller.Search(2021) as OkObjectResult;
        Assert.IsNotNull(result);
        var albums = result.Value as List<Album>;
        Assert.IsNotNull(albums);
        Assert.IsTrue(albums.Count > 0);
        Assert.IsTrue(albums.All(a => a.Year == 2021));
    }

    [TestMethod]
    public void Search_NoMatchForYear_ReturnsEmptyList()
    {
        var result = _controller.Search(1900) as OkObjectResult;
        Assert.IsNotNull(result);
        var albums = result.Value as List<Album>;
        Assert.IsNotNull(albums);
        Assert.AreEqual(0, albums.Count);
    }

    // --- Sort ---

    [TestMethod]
    public void Sort_MissingSortBy_Returns400()
    {
        var result = _controller.Get(null as string);
        Assert.IsInstanceOfType(result, typeof(BadRequestObjectResult));
    }

    [TestMethod]
    public void Sort_InvalidSortBy_Returns400()
    {
        var result = _controller.Get("invalid");
        Assert.IsInstanceOfType(result, typeof(BadRequestObjectResult));
    }

    [TestMethod]
    public void Sort_ByTitle_Returns200()
    {
        var result = _controller.Get("title") as OkObjectResult;
        Assert.IsNotNull(result);
        Assert.AreEqual(200, result.StatusCode);
    }

    [TestMethod]
    public void Sort_ByArtist_ReturnsAlbumsOrderedByArtistName()
    {
        var result = _controller.Get("artist") as OkObjectResult;
        Assert.IsNotNull(result);
        var albums = result.Value as List<Album>;
        Assert.IsNotNull(albums);

        var names = albums.Select(a => a.Artist.Name).ToList();
        var expected = names.OrderBy(n => n).ToList();
        CollectionAssert.AreEqual(expected, names);
    }

    // --- POST / Create ---

    [TestMethod]
    public void Post_ValidRequest_Returns201WithNewAlbum()
    {
        var request = new CreateAlbumRequest("New Album", new Artist("New Artist", new DateOnly(1995, 1, 1), "London"), 2023, 9.99, "https://example.com/img.png");
        var result = _controller.Post(request) as CreatedAtActionResult;
        Assert.IsNotNull(result);
        Assert.AreEqual(201, result.StatusCode);
        var album = result.Value as Album;
        Assert.IsNotNull(album);
        Assert.AreEqual("New Album", album.Title);
        Assert.AreEqual("New Artist", album.Artist.Name);
    }

    [TestMethod]
    public void Post_ValidRequest_PersistsArtistDetails()
    {
        var artist = new Artist("Detail Artist", new DateOnly(1990, 12, 31), "Paris");
        var request = new CreateAlbumRequest("Details Album", artist, 2025, 12.5, "https://example.com/details.png");

        var postResult = _controller.Post(request) as CreatedAtActionResult;
        Assert.IsNotNull(postResult);
        var created = postResult.Value as Album;
        Assert.IsNotNull(created);

        var getResult = _controller.Get(created.Id) as OkObjectResult;
        Assert.IsNotNull(getResult);
        var fetched = getResult.Value as Album;
        Assert.IsNotNull(fetched);
        Assert.AreEqual("Detail Artist", fetched.Artist.Name);
        Assert.AreEqual(new DateOnly(1990, 12, 31), fetched.Artist.Birthdate);
        Assert.AreEqual("Paris", fetched.Artist.BirthPlace);
    }

    [TestMethod]
    public void Post_NewAlbum_IsRetrievableAfterCreation()
    {
        var request = new CreateAlbumRequest("Also New", new Artist("Artist X", new DateOnly(1998, 6, 15), "Madrid"), 2024, 11.99, "https://example.com/img.png");
        var postResult = _controller.Post(request) as CreatedAtActionResult;
        var created = postResult!.Value as Album;

        var getResult = _controller.Get(created!.Id) as OkObjectResult;
        Assert.IsNotNull(getResult);
        var fetched = getResult.Value as Album;
        Assert.AreEqual("Also New", fetched!.Title);
    }

    // --- PUT / Update ---

    [TestMethod]
    public void Put_ExistingId_Returns200WithUpdatedAlbum()
    {
        var request = new UpdateAlbumRequest("Updated Title", new Artist("Updated Artist", new DateOnly(1993, 9, 9), "Oslo"), 2023, 15.99, "https://example.com/upd.png");
        var result = _controller.Put(1, request) as OkObjectResult;
        Assert.IsNotNull(result);
        var album = result.Value as Album;
        Assert.IsNotNull(album);
        Assert.AreEqual("Updated Title", album.Title);
        Assert.AreEqual("Updated Artist", album.Artist.Name);
    }

    [TestMethod]
    public void Put_UnknownId_Returns404()
    {
        var request = new UpdateAlbumRequest("X", new Artist("Y", new DateOnly(2000, 1, 1), "Rome"), 2000, 1.0, "img");
        var result = _controller.Put(999, request);
        Assert.IsInstanceOfType(result, typeof(NotFoundResult));
    }

    // --- DELETE ---

    [TestMethod]
    public void Delete_ExistingId_Returns204()
    {
        var result = _controller.Delete(1);
        Assert.IsInstanceOfType(result, typeof(NoContentResult));
    }

    [TestMethod]
    public void Delete_ExistingId_RemovesAlbum()
    {
        _controller.Delete(1);
        var getResult = _controller.Get(1);
        Assert.IsInstanceOfType(getResult, typeof(NotFoundResult));
    }

    [TestMethod]
    public void Delete_UnknownId_Returns404()
    {
        var result = _controller.Delete(999);
        Assert.IsInstanceOfType(result, typeof(NotFoundResult));
    }
}
