using System;
using Xunit;
using albums_api.Models;
using System.Linq;

namespace albums_api.Tests
{
    public class AlbumTests
    {
        [Fact]
        public void GetAll_ReturnsAllAlbums()
        {
            var albums = Album.GetAll();
            Assert.NotNull(albums);
            Assert.True(albums.Count >= 1);
        }

        [Fact]
        public void GetById_ReturnsCorrectAlbum()
        {
            var album = Album.GetById(1);
            Assert.NotNull(album);
            Assert.Equal(1, album.Id);
        }

        [Fact]
        public void Create_AddsNewAlbum()
        {
            var artist = new Artist("Test Artist", new DateTime(2000, 1, 1), "Test City");
            var newAlbum = new Album(0, "Test Album", artist, 2026, 9.99, "test-url");
            var created = Album.Create(newAlbum);
            Assert.NotNull(created);
            Assert.True(created.Id > 0);
            Assert.Equal("Test Album", created.Title);
        }

        [Fact]
        public void Update_UpdatesExistingAlbum()
        {
            var artist = new Artist("Update Artist", new DateTime(1999, 2, 2), "Update City");
            var updated = new Album(0, "Updated Title", artist, 2027, 19.99, "update-url");
            var result = Album.Update(1, updated);
            Assert.True(result);
            var album = Album.GetById(1);
            Assert.Equal("Updated Title", album.Title);
        }

        [Fact]
        public void Delete_RemovesAlbum()
        {
            var artist = new Artist("Delete Artist", new DateTime(1998, 3, 3), "Delete City");
            var album = Album.Create(new Album(0, "Delete Album", artist, 2028, 29.99, "delete-url"));
            var result = Album.Delete(album.Id);
            Assert.True(result);
            var deleted = Album.GetById(album.Id);
            Assert.Null(deleted);
        }

        [Fact]
        public void SearchByYear_ReturnsCorrectAlbums()
        {
            var year = 2022;
            var results = Album.SearchByYear(year);
            Assert.All(results, a => Assert.Equal(year, a.Year));
        }
    }
}
