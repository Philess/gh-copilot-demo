namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, int Year, double Price, string Image_url)
    {
        private static List<Album> _albums = new List<Album>
        {
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateOnly(1992, 5, 14), "Seattle"), 2020, 10.99, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateOnly(1988, 10, 21), "Austin"), 2021, 13.99, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateOnly(1990, 2, 7), "Dublin"), 2022, 13.99, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateOnly(1985, 7, 2), "Amsterdam"), 2020, 12.99, "https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateOnly(1991, 11, 30), "Berlin"), 2021, 12.99, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateOnly(1987, 3, 19), "Chicago"), 2022, 14.99, "https://aka.ms/albums-containerappslogo")
        };

        private static int _nextId = 7;

        public static List<Album> GetAll() => new List<Album>(_albums);

        public static Album? GetById(int id) => _albums.FirstOrDefault(a => a.Id == id);

        public static List<Album> GetByYear(int year) =>
            _albums.Where(a => a.Year == year).ToList();

        public static Album Create(string title, Artist artist, int year, double price, string imageUrl)
        {
            var album = new Album(_nextId++, title, artist, year, price, imageUrl);
            _albums.Add(album);
            return album;
        }

        public static Album? Update(int id, string title, Artist artist, int year, double price, string imageUrl)
        {
            var index = _albums.FindIndex(a => a.Id == id);
            if (index < 0) return null;
            var updated = new Album(id, title, artist, year, price, imageUrl);
            _albums[index] = updated;
            return updated;
        }

        public static bool Delete(int id)
        {
            var album = _albums.FirstOrDefault(a => a.Id == id);
            if (album is null) return false;
            _albums.Remove(album);
            return true;
        }
    }
}
