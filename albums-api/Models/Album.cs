namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, int year, double Price, string Image_url)
    {
        // Backing store shared across requests so that create/update/delete operations persist
        // for the lifetime of the running application.
        private static readonly List<Album> _albums = new List<Album>(){
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateTime(2019, 10, 16), "Redmond, WA"), 2023, 10.99, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateTime(2018, 3, 12), "San Francisco, CA"), 2023, 13.99, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateTime(2019, 5, 24), "Seattle, WA"), 2023, 13.99, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateTime(2017, 8, 1), "Austin, TX"), 2023, 12.99,"https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateTime(2016, 11, 9), "Boston, MA"), 2023, 12.99, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateTime(2015, 6, 20), "Chicago, IL"), 2023, 14.99, "https://aka.ms/albums-containerappslogo")
         };

        // Guards all reads/writes of the shared list above, since ASP.NET Core can process
        // multiple requests concurrently on different threads.
        private static readonly object _lock = new object();

        public static List<Album> GetAll()
        {
            lock (_lock)
            {
                return new List<Album>(_albums);
            }
        }

        public static Album? GetById(int id)
        {
            lock (_lock)
            {
                return _albums.FirstOrDefault(a => a.Id == id);
            }
        }

        // function that retrieves albums released in a given year
        public static List<Album> GetByYear(int year)
        {
            lock (_lock)
            {
                return _albums.Where(a => a.year == year).ToList();
            }
        }

        public static Album Create(Album album)
        {
            lock (_lock)
            {
                var nextId = _albums.Count == 0 ? 1 : _albums.Max(a => a.Id) + 1;
                var newAlbum = album with { Id = nextId };
                _albums.Add(newAlbum);
                return newAlbum;
            }
        }

        public static Album? Update(int id, Album album)
        {
            lock (_lock)
            {
                var index = _albums.FindIndex(a => a.Id == id);
                if (index == -1)
                {
                    return null;
                }

                var updatedAlbum = album with { Id = id };
                _albums[index] = updatedAlbum;
                return updatedAlbum;
            }
        }

        public static bool Delete(int id)
        {
            lock (_lock)
            {
                var album = _albums.FirstOrDefault(a => a.Id == id);
                if (album is null)
                {
                    return false;
                }

                return _albums.Remove(album);
            }
        }
    }
}
