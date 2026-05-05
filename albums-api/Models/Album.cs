namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, double Price, string Image_url, int Year)
    {
        private static readonly List<Album> albums = new()
        {
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateTime(1998, 5, 12), "Seattle, USA"), 10.99, "https://aka.ms/albums-daprlogo", 2022),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateTime(1995, 9, 3), "Detroit, USA"), 13.99, "https://aka.ms/albums-containerappslogo", 2023),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateTime(2000, 2, 19), "Berlin, Germany"), 13.99, "https://aka.ms/albums-kedalogo", 2024),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateTime(1992, 11, 27), "Paris, France"), 12.99, "https://aka.ms/albums-envoylogo", 2021),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateTime(1997, 7, 14), "Toronto, Canada"), 12.99, "https://aka.ms/albums-vnetlogo", 2023),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateTime(1990, 1, 8), "London, UK"), 14.99, "https://aka.ms/albums-containerappslogo", 2020)
        };

        public static List<Album> GetAll()
        {
            return albums;
        }

        public static Album? GetById(int id)
        {
            return albums.FirstOrDefault(a => a.Id == id);
        }

        public static List<Album> GetByYear(int year)
        {
            return albums.Where(a => a.Year == year).ToList();
        }

        public static Album Create(Album album)
        {
            var nextId = albums.Count == 0 ? 1 : albums.Max(a => a.Id) + 1;
            var createdAlbum = album with { Id = nextId };
            albums.Add(createdAlbum);

            return createdAlbum;
        }

        public static Album? Update(int id, Album album)
        {
            var existingAlbum = GetById(id);
            if (existingAlbum is null)
            {
                return null;
            }

            var index = albums.FindIndex(a => a.Id == id);
            var updatedAlbum = album with { Id = id };
            albums[index] = updatedAlbum;

            return updatedAlbum;
        }

        public static bool Delete(int id)
        {
            var existingAlbum = GetById(id);
            if (existingAlbum is null)
            {
                return false;
            }

            albums.Remove(existingAlbum);
            return true;
        }
    }
}
