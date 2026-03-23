namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, int Year, double Price, string Image_url)
    {
        private static readonly object SyncRoot = new();
        private static readonly List<Album> Albums =
        [
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateOnly(1988, 5, 14), "Seattle, USA"), 2020, 10.99, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateOnly(1992, 8, 21), "Portland, USA"), 2021, 13.99, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateOnly(1985, 11, 3), "Austin, USA"), 2022, 13.99, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateOnly(1990, 2, 17), "Dublin, Ireland"), 2020, 12.99, "https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateOnly(1987, 7, 9), "London, UK"), 2021, 12.99, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateOnly(1994, 1, 28), "Toronto, Canada"), 2022, 14.99, "https://aka.ms/albums-containerappslogo")
        ];

        public static List<Album> GetAll()
        {
            lock (SyncRoot)
            {
                return Albums.ToList();
            }
        }

        public static Album? GetById(int id)
        {
            lock (SyncRoot)
            {
                return Albums.FirstOrDefault(album => album.Id == id);
            }
        }

        public static List<Album> GetByYear(int year)
        {
            lock (SyncRoot)
            {
                return Albums.Where(album => album.Year == year).ToList();
            }
        }

        public static Album Create(AlbumRequest request)
        {
            lock (SyncRoot)
            {
                var nextId = Albums.Count == 0 ? 1 : Albums.Max(album => album.Id) + 1;
                var album = new Album(nextId, request.Title, request.Artist, request.Year, request.Price, request.Image_url);
                Albums.Add(album);
                return album;
            }
        }

        public static Album? Update(int id, AlbumRequest request)
        {
            lock (SyncRoot)
            {
                var index = Albums.FindIndex(album => album.Id == id);
                if (index == -1)
                {
                    return null;
                }

                var updatedAlbum = new Album(id, request.Title, request.Artist, request.Year, request.Price, request.Image_url);
                Albums[index] = updatedAlbum;
                return updatedAlbum;
            }
        }

        public static bool Delete(int id)
        {
            lock (SyncRoot)
            {
                var index = Albums.FindIndex(album => album.Id == id);
                if (index == -1)
                {
                    return false;
                }

                Albums.RemoveAt(index);
                return true;
            }
        }
    }

    public record AlbumRequest(string Title, Artist Artist, int Year, double Price, string Image_url);
}
