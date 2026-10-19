using System.Net;
using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using albums_api.Contracts;
using Microsoft.AspNetCore.Mvc.Testing;
using Xunit;

namespace albums_api.Tests;

public sealed class AlbumApiTests : IDisposable
{
    private readonly WebApplicationFactory<Program> factory = new();
    private readonly HttpClient client;

    public AlbumApiTests()
    {
        client = factory.CreateClient(new WebApplicationFactoryClientOptions
        {
            BaseAddress = new Uri("https://localhost")
        });
    }

    private static JsonObject Payload() => new()
    {
        ["title"] = "Test Album",
        ["artist"] = "Test Artist",
        ["year"] = 2030,
        ["price"] = 9.99,
        ["image_url"] = "https://example.com/cover.png"
    };

    [Fact]
    public async Task CrudFlowSharesStateAcrossAllReads()
    {
        using var created = await client.PostAsJsonAsync("/albums", Payload());
        Assert.Equal(HttpStatusCode.Created, created.StatusCode);
        var album = await ReadAlbum(created);
        Assert.Equal(7, album.Id);
        Assert.Equal(2030, album.Year);
        var location = Assert.IsType<Uri>(created.Headers.Location);
        using var detail = await client.GetAsync(location);
        Assert.Equal(HttpStatusCode.OK, detail.StatusCode);
        Assert.Equal(album, await ReadAlbum(detail));

        using (var json = JsonDocument.Parse(await created.Content.ReadAsStringAsync()))
        {
            Assert.Equal(new[] { "artist", "id", "image_url", "price", "title", "year" },
                json.RootElement.EnumerateObject().Select(p => p.Name).OrderBy(n => n).ToArray());
        }

        Assert.Contains(await ReadAlbums("/albums"), a => a.Id == album.Id);
        Assert.Contains(await ReadAlbums("/albums/sort?sortBy=title"), a => a.Id == album.Id);
        Assert.Equal(album, Assert.Single(await ReadAlbums("/albums/search?year=2030")));

        var replacement = Payload();
        replacement["id"] = 999;
        replacement["title"] = "Updated Album";
        replacement["artist"] = "Updated Artist";
        replacement["year"] = 2031;
        replacement["price"] = 0;
        replacement["image_url"] = "http://example.com/updated.png";
        using var updated = await client.PutAsJsonAsync($"/albums/{album.Id}", replacement);
        Assert.Equal(HttpStatusCode.OK, updated.StatusCode);
        var updatedAlbum = await ReadAlbum(updated);
        Assert.Equal(new AlbumResponse(album.Id, "Updated Album", "Updated Artist", 2031, 0,
            "http://example.com/updated.png"), updatedAlbum);
        Assert.Empty(await ReadAlbums("/albums/search?year=2030"));
        Assert.Equal(updatedAlbum, Assert.Single(await ReadAlbums("/albums/search?year=2031")));
        Assert.Contains(updatedAlbum, await ReadAlbums("/albums"));
        Assert.Contains(updatedAlbum, await ReadAlbums("/albums/sort?sortBy=price"));
        using var updatedDetail = await client.GetAsync(location);
        Assert.Equal(updatedAlbum, await ReadAlbum(updatedDetail));

        using var deleted = await client.DeleteAsync($"/albums/{album.Id}");
        Assert.Equal(HttpStatusCode.NoContent, deleted.StatusCode);
        Assert.Equal("", await deleted.Content.ReadAsStringAsync());
        using var missing = await client.GetAsync(location);
        await AssertProblem(missing, HttpStatusCode.NotFound);
        using var deletedAgain = await client.DeleteAsync($"/albums/{album.Id}");
        await AssertProblem(deletedAgain, HttpStatusCode.NotFound);
        Assert.DoesNotContain(await ReadAlbums("/albums"), a => a.Id == album.Id);
        Assert.DoesNotContain(await ReadAlbums("/albums/sort?sortBy=title"), a => a.Id == album.Id);
        Assert.Empty(await ReadAlbums("/albums/search?year=2031"));

        using var next = await client.PostAsJsonAsync("/albums", Payload());
        Assert.Equal(8, (await ReadAlbum(next)).Id);
    }

    [Fact]
    public async Task SeedsAndExactYearSearchRemainAvailable()
    {
        var albums = await ReadAlbums("/albums");
        Assert.Equal(Enumerable.Range(1, 6), albums.Select(a => a.Id));
        Assert.Equal(Enumerable.Range(2020, 6), albums.Select(a => a.Year));
        Assert.Equal(new AlbumResponse(1, "You, Me and an App Id", "Daprize", 2020, 10.99,
            "https://aka.ms/albums-daprlogo"), albums[0]);
        using var detail = await client.GetAsync("/albums/1");
        Assert.Equal(albums[0], await ReadAlbum(detail));
        Assert.Equal(albums[4], Assert.Single(await ReadAlbums("/albums/search?year=2024")));
        Assert.Empty(await ReadAlbums("/albums/search?year=1999"));
    }

    [Theory]
    [InlineData("title")]
    [InlineData("artist")]
    [InlineData("price")]
    [InlineData("TITLE")]
    public async Task SortModesKeepTheirExistingBehavior(string sortBy)
    {
        var albums = await ReadAlbums("/albums");
        var expected = sortBy.ToLowerInvariant() switch
        {
            "title" => albums.OrderBy(a => a.Title),
            "artist" => albums.OrderBy(a => a.Artist),
            _ => albums.OrderBy(a => a.Price)
        };
        Assert.Equal(expected, await ReadAlbums($"/albums/sort?sortBy={sortBy}"));
    }

    [Fact]
    public async Task UnsupportedSortRemainsBadRequest()
    {
        using var response = await client.GetAsync("/albums/sort?sortBy=unknown");
        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
        Assert.Contains("Invalid sort parameter", await response.Content.ReadAsStringAsync());
    }

    [Theory]
    [InlineData("")]
    [InlineData("?year=")]
    [InlineData("?year=abc")]
    [InlineData("?year=2024.5")]
    [InlineData("?year=0")]
    [InlineData("?year=-1")]
    [InlineData("?year=10000")]
    public async Task InvalidSearchYearsReturnValidationProblems(string query)
    {
        using var response = await client.GetAsync("/albums/search" + query);
        await AssertProblem(response, HttpStatusCode.BadRequest);
    }

    [Theory]
    [InlineData(1)]
    [InlineData(9999)]
    public async Task YearBoundariesAndZeroPriceAreValid(int year)
    {
        var payload = Payload();
        payload["year"] = year;
        payload["price"] = 0;
        using var response = await client.PostAsJsonAsync("/albums", payload);
        Assert.Equal(HttpStatusCode.Created, response.StatusCode);
        Assert.Equal(year, (await ReadAlbum(response)).Year);
        Assert.Single(await ReadAlbums($"/albums/search?year={year}"));
    }

    public static IEnumerable<object[]> InvalidPayloads()
    {
        foreach (string field in new[] { "title", "artist", "year", "price", "image_url" })
        {
            var missing = Payload();
            missing.Remove(field);
            yield return new object[] { missing.ToJsonString() };
            var nullValue = Payload();
            nullValue[field] = null;
            yield return new object[] { nullValue.ToJsonString() };
        }

        foreach (var (field, value) in new (string, JsonNode?)[]
        {
            ("title", JsonValue.Create(" ")), ("artist", JsonValue.Create("")),
            ("year", JsonValue.Create(0)), ("year", JsonValue.Create(10000)),
            ("price", JsonValue.Create(-0.01)), ("image_url", JsonValue.Create("")),
            ("image_url", JsonValue.Create("/relative.png")),
            ("image_url", JsonValue.Create("ftp://example.com/cover.png")),
            ("image_url", JsonValue.Create("https://"))
        })
        {
            var invalid = Payload();
            invalid[field] = value;
            yield return new object[] { invalid.ToJsonString() };
        }

        foreach (string price in new[] { "\"NaN\"", "\"Infinity\"", "\"-Infinity\"", "1e999" })
            yield return new object[] { Payload().ToJsonString().Replace("\"price\":9.99", $"\"price\":{price}") };

        yield return new object[] { "null" };
        yield return new object[] { "{invalid json}" };
    }

    [Theory]
    [MemberData(nameof(InvalidPayloads))]
    public async Task InvalidWritesDoNotMutateState(string payload)
    {
        var before = await ReadAlbums("/albums");
        foreach (var method in new[] { HttpMethod.Post, HttpMethod.Put })
        {
            using var request = new HttpRequestMessage(method, method == HttpMethod.Post ? "/albums" : "/albums/1")
            {
                Content = new StringContent(payload, Encoding.UTF8, "application/json")
            };
            using var response = await client.SendAsync(request);
            await AssertProblem(response, HttpStatusCode.BadRequest);
        }
        Assert.Equal(before, await ReadAlbums("/albums"));
    }

    [Theory]
    [InlineData("GET")]
    [InlineData("PUT")]
    [InlineData("DELETE")]
    public async Task UnknownIdsReturnNotFoundWithoutUpserting(string method)
    {
        using var request = new HttpRequestMessage(new HttpMethod(method), "/albums/999");
        if (method == "PUT")
            request.Content = JsonContent.Create(Payload());
        using var response = await client.SendAsync(request);
        await AssertProblem(response, HttpStatusCode.NotFound);
        Assert.Equal(6, (await ReadAlbums("/albums")).Length);
    }

    [Fact]
    public async Task ConcurrentCreatesHaveUniqueServerAssignedIdsAndPermitDuplicateTitles()
    {
        var responses = await Task.WhenAll(Enumerable.Range(0, 32)
            .Select(_ => client.PostAsJsonAsync("/albums", Payload())));
        try
        {
            Assert.All(responses, r => Assert.Equal(HttpStatusCode.Created, r.StatusCode));
            var albums = await Task.WhenAll(responses.Select(ReadAlbum));
            Assert.Equal(Enumerable.Range(7, 32), albums.Select(a => a.Id).OrderBy(id => id));
            Assert.Equal(38, (await ReadAlbums("/albums")).Length);
            Assert.Equal(32, (await ReadAlbums("/albums/search?year=2030")).Length);
        }
        finally
        {
            foreach (var response in responses)
                response.Dispose();
        }
    }

    [Fact]
    public async Task SwaggerDocumentsEveryAlbumRouteAndResponseContract()
    {
        using var response = await client.GetAsync("/swagger/v1/swagger.json");
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        using var document = JsonDocument.Parse(await response.Content.ReadAsStringAsync());
        var paths = document.RootElement.GetProperty("paths");
        Assert.True(paths.GetProperty("/albums").TryGetProperty("post", out var post));
        Assert.True(paths.GetProperty("/albums/{id}").TryGetProperty("put", out _));
        Assert.True(paths.GetProperty("/albums/{id}").TryGetProperty("delete", out _));
        Assert.True(paths.GetProperty("/albums/search").TryGetProperty("get", out var search));
        Assert.True(paths.GetProperty("/albums/sort").TryGetProperty("get", out _));
        Assert.True(post.TryGetProperty("summary", out _));
        Assert.True(post.GetProperty("responses").TryGetProperty("201", out _));
        Assert.True(paths.GetProperty("/albums/{id}").GetProperty("delete")
            .GetProperty("responses").TryGetProperty("204", out _));
        Assert.True(search.GetProperty("parameters")[0].GetProperty("required").GetBoolean());
        var schemas = document.RootElement.GetProperty("components").GetProperty("schemas");
        foreach (string name in new[] { "CreateAlbumRequest", "UpdateAlbumRequest" })
        {
            Assert.Equal(new[] { "artist", "image_url", "price", "title", "year" },
                schemas.GetProperty(name).GetProperty("required").EnumerateArray()
                    .Select(field => field.GetString()).OrderBy(field => field).ToArray());
        }
        var schema = schemas.GetProperty("AlbumResponse").GetProperty("properties");
        Assert.True(schema.TryGetProperty("image_url", out _));
        Assert.True(schema.TryGetProperty("year", out _));
    }

    private async Task<AlbumResponse[]> ReadAlbums(string path)
    {
        using var response = await client.GetAsync(path);
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        return Assert.IsType<AlbumResponse[]>(await response.Content.ReadFromJsonAsync<AlbumResponse[]>());
    }

    private static async Task<AlbumResponse> ReadAlbum(HttpResponseMessage response) =>
        Assert.IsType<AlbumResponse>(await response.Content.ReadFromJsonAsync<AlbumResponse>());

    private static async Task AssertProblem(HttpResponseMessage response, HttpStatusCode expected)
    {
        Assert.Equal(expected, response.StatusCode);
        Assert.Equal("application/problem+json", response.Content.Headers.ContentType?.MediaType);
        using var problem = JsonDocument.Parse(await response.Content.ReadAsStringAsync());
        Assert.Equal((int)expected, problem.RootElement.GetProperty("status").GetInt32());
    }

    public void Dispose()
    {
        client.Dispose();
        factory.Dispose();
    }
}
