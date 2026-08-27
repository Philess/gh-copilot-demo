namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, int Year, double Price, string Image_url)
    {
        private static readonly object SyncRoot = new();
        private static readonly List<Album> Albums = new()
        {
            new(1, "You, Me and an App Id", new Artist("Daprize", new DateOnly(1988, 4, 12), "Seattle"), 2021, 10.99, "https://aka.ms/albums-daprlogo"),
            new(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateOnly(1985, 7, 23), "Detroit"), 2020, 13.99, "https://aka.ms/albums-containerappslogo"),
            new(3, "Scale It Up", new Artist("KEDA Club", new DateOnly(1990, 2, 8), "Chicago"), 2019, 13.99, "https://aka.ms/albums-kedalogo"),
            new(4, "Lost in Translation", new Artist("MegaDNS", new DateOnly(1982, 11, 19), "London"), 2018, 12.99, "https://aka.ms/albums-envoylogo"),
            new(5, "Lock Down Your Love", new Artist("V is for VNET", new DateOnly(1987, 6, 4), "Toronto"), 2017, 12.99, "https://aka.ms/albums-vnetlogo"),
            new(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateOnly(1981, 9, 15), "Los Angeles"), 2016, 14.99, "https://aka.ms/albums-containerappslogo")
        };

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

        public static Album Create(Album album)
        {
            lock (SyncRoot)
            {
                int id = Albums.Count == 0 ? 1 : Albums.Max(existing => existing.Id) + 1;
                Album createdAlbum = album with { Id = id };
                Albums.Add(createdAlbum);
                return createdAlbum;
            }
        }

        public static Album? Update(int id, Album album)
        {
            lock (SyncRoot)
            {
                int index = Albums.FindIndex(existing => existing.Id == id);
                if (index < 0)
                {
                    return null;
                }

                Album updatedAlbum = album with { Id = id };
                Albums[index] = updatedAlbum;
                return updatedAlbum;
            }
        }

        public static bool Delete(int id)
        {
            lock (SyncRoot)
            {
                return Albums.RemoveAll(album => album.Id == id) > 0;
            }
        }
    }
}
