namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, int Year, double Price, string Image_url)
    {
        // In-memory static list for CRUD operations
        private static List<Album> _albums = new List<Album>(){
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateTime(1990, 1, 1), "Seattle, WA"), 2020, 10.99, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateTime(1985, 5, 12), "London, UK"), 2021, 13.99, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateTime(1992, 7, 23), "Berlin, Germany"), 2022, 13.99, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateTime(1988, 3, 15), "Tokyo, Japan"), 2023, 12.99,"https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateTime(1995, 11, 30), "New York, NY"), 2024, 12.99, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateTime(1980, 6, 6), "Los Angeles, CA"), 2025, 14.99, "https://aka.ms/albums-containerappslogo")
        };

        public static List<Album> GetAll()
        {
            return _albums.ToList();
        }

        public static Album? GetById(int id)
        {
            return _albums.FirstOrDefault(a => a.Id == id);
        }

        public static List<Album> SearchByYear(int year)
        {
            return _albums.Where(a => a.Year == year).ToList();
        }

        public static Album Create(Album album)
        {
            int newId = _albums.Any() ? _albums.Max(a => a.Id) + 1 : 1;
            var newAlbum = album with { Id = newId };
            _albums.Add(newAlbum);
            return newAlbum;
        }

        public static bool Update(int id, Album updated)
        {
            var idx = _albums.FindIndex(a => a.Id == id);
            if (idx == -1) return false;
            _albums[idx] = updated with { Id = id };
            return true;
        }

        public static bool Delete(int id)
        {
            var album = _albums.FirstOrDefault(a => a.Id == id);
            if (album == null) return false;
            _albums.Remove(album);
            return true;
        }
    }
}
