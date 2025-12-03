using albums_api.Models;
using FluentAssertions;
using Xunit;

namespace albums_api.Tests;

public class AlbumTests
{
    [Fact]
    public void GetAll_ShouldReturnSeededAlbumSet()
    {
        var result = Album.GetAll();
        result.Should().HaveCount(6);
        result.Select(a => a.Title).Should().Contain("You, Me and an App Id");
    }

    [Theory]
    [InlineData(1, "Daprize")]
    [InlineData(6, "Guns N Probeses")]
    public void GetById_ShouldReturnAlbumWhenFound(int id, string expectedArtist)
    {
        var album = Album.GetById(id);
        album.Should().NotBeNull();
        album!.Artist.Should().Be(expectedArtist);
    }

    [Fact]
    public void GetById_ShouldReturnNullWhenMissing()
    {
        var album = Album.GetById(-1);
        album.Should().BeNull();
    }
}