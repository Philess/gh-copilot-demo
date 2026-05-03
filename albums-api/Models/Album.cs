namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, double Price, int Year, string Image_url)
    {
        // In-memory storage for albums
        private static List<Album> _albums = new List<Album>()
        {
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateTime(1995, 3, 15), "Seattle, WA"), 10.99, 2021, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateTime(1990, 7, 22), "Detroit, MI"), 13.99, 2020, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateTime(1998, 11, 5), "Austin, TX"), 13.99, 2021, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateTime(1992, 4, 18), "San Francisco, CA"), 12.99, 2020, "https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateTime(1996, 9, 30), "Redmond, WA"), 12.99, 2021, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateTime(1988, 6, 12), "Los Angeles, CA"), 14.99, 2021, "https://aka.ms/albums-containerappslogo")
        };

        private static int _nextId = 7;

        public static List<Album> GetAll()
        {
            return _albums.ToList();
        }

        public static Album? GetById(int id)
        {
            return _albums.FirstOrDefault(a => a.Id == id);
        }

        public static List<Album> GetByYear(int year)
        {
            return _albums.Where(a => a.Year == year).ToList();
        }

        public static Album Create(string title, Artist artist, double price, int year, string imageUrl)
        {
            var album = new Album(_nextId++, title, artist, price, year, imageUrl);
            _albums.Add(album);
            return album;
        }

        public static bool Update(int id, string title, Artist artist, double price, int year, string imageUrl)
        {
            var existingAlbum = GetById(id);
            if (existingAlbum is null)
            {
                return false;
            }

            _albums.Remove(existingAlbum);
            var updatedAlbum = new Album(id, title, artist, price, year, imageUrl);
            _albums.Add(updatedAlbum);
            return true;
        }

        public static bool Delete(int id)
        {
            var album = GetById(id);
            if (album is null)
            {
                return false;
            }

            _albums.Remove(album);
            return true;
        }
    }
}
