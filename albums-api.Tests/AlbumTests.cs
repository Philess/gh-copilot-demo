using albums_api.Models;

namespace albums_api.Tests
{
    public class AlbumTests
    {
        [Fact]
        public void GetAll_ReturnsSeededAlbums()
        {
            var albums = Album.GetAll();

            Assert.NotEmpty(albums);
            Assert.Contains(albums, a => a.Title == "You, Me and an App Id" && a.Artist.Name == "Daprize");
        }

        [Fact]
        public void GetById_ReturnsMatchingAlbum()
        {
            var album = Album.GetById(1);

            Assert.NotNull(album);
            Assert.Equal("You, Me and an App Id", album!.Title);
            Assert.Equal("Daprize", album.Artist.Name);
        }

        [Fact]
        public void GetById_ReturnsNullForUnknownId()
        {
            var album = Album.GetById(int.MaxValue);

            Assert.Null(album);
        }

        [Fact]
        public void GetByYear_ReturnsAlbumsFromThatYear()
        {
            var albums = Album.GetByYear(2023);

            Assert.NotEmpty(albums);
            Assert.All(albums, a => Assert.Equal(2023, a.year));
        }

        [Fact]
        public void GetByYear_ReturnsEmptyListWhenNoAlbumsMatch()
        {
            var albums = Album.GetByYear(1800);

            Assert.Empty(albums);
        }

        [Fact]
        public void Create_AssignsNewIdAndAddsAlbum()
        {
            var artist = new Artist("Unit Test Artist", new DateTime(2000, 1, 1), "Test City");
            var newAlbum = new Album(0, "Unit Test Album", artist, 1999, 5.99, "http://example.com/image.png");

            var created = Album.Create(newAlbum);

            Assert.True(created.Id > 0);
            Assert.Equal("Unit Test Album", created.Title);

            var fetched = Album.GetById(created.Id);
            Assert.NotNull(fetched);
            Assert.Equal(created, fetched);
        }

        [Fact]
        public void Update_ModifiesExistingAlbum()
        {
            var artist = new Artist("Original Artist", new DateTime(2001, 2, 3), "Original City");
            var created = Album.Create(new Album(0, "Original Title", artist, 2010, 1.99, "http://example.com/original.png"));

            var updatedArtist = new Artist("Updated Artist", new DateTime(2002, 3, 4), "Updated City");
            var updated = Album.Update(created.Id, new Album(0, "Updated Title", updatedArtist, 2011, 2.99, "http://example.com/updated.png"));

            Assert.NotNull(updated);
            Assert.Equal(created.Id, updated!.Id);
            Assert.Equal("Updated Title", updated.Title);
            Assert.Equal("Updated Artist", updated.Artist.Name);

            var fetched = Album.GetById(created.Id);
            Assert.Equal(updated, fetched);
        }

        [Fact]
        public void Update_ReturnsNullForUnknownId()
        {
            var artist = new Artist("Nobody", new DateTime(2000, 1, 1), "Nowhere");
            var result = Album.Update(int.MaxValue, new Album(0, "Does Not Exist", artist, 2000, 0.99, "http://example.com/none.png"));

            Assert.Null(result);
        }

        [Fact]
        public void Delete_RemovesExistingAlbum()
        {
            var artist = new Artist("Delete Me", new DateTime(2005, 5, 5), "Delete City");
            var created = Album.Create(new Album(0, "Delete Me Album", artist, 2020, 3.99, "http://example.com/delete.png"));

            var deleted = Album.Delete(created.Id);

            Assert.True(deleted);
            Assert.Null(Album.GetById(created.Id));
        }

        [Fact]
        public void Delete_ReturnsFalseForUnknownId()
        {
            var deleted = Album.Delete(int.MaxValue);

            Assert.False(deleted);
        }
    }
}
