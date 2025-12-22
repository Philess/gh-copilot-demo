namespace albums_api.Models
{
    public record Album(int Id, string Title, Artist Artist, int Year, double Price, string Image_url)
    {
        /// <summary>
        /// Get all albums
        /// </summary>
        /// <returns></returns>
        public static List<Album> GetAll()
        {
            var albums = new List<Album>(){
            new Album(1, "You, Me and an App Id", new Artist("Daprize", new DateTime(2015, 5, 10), "Seattle"), 2020, 10.99, "https://aka.ms/albums-daprlogo"),
            new Album(2, "Seven Revision Army", new Artist("The Blue-Green Stripes", new DateTime(2010, 3, 15), "Portland"), 2021, 13.99, "https://aka.ms/albums-containerappslogo"),
            new Album(3, "Scale It Up", new Artist("KEDA Club", new DateTime(2018, 7, 20), "San Francisco"), 2022, 13.99, "https://aka.ms/albums-kedalogo"),
            new Album(4, "Lost in Translation", new Artist("MegaDNS", new DateTime(2012, 1, 8), "Austin"), 2023, 12.99,"https://aka.ms/albums-envoylogo"),
            new Album(5, "Lock Down Your Love", new Artist("V is for VNET", new DateTime(2019, 9, 12), "Denver"), 2024, 12.99, "https://aka.ms/albums-vnetlogo"),
            new Album(6, "Sweet Container O' Mine", new Artist("Guns N Probeses", new DateTime(2008, 11, 3), "Los Angeles"), 2025, 14.99, "https://aka.ms/albums-containerappslogo")
         };

            return albums;
        }

/// <summary>
/// Get album by ID
/// </summary>
/// <param name="id"></param>
/// <returns></returns>
        public static Album? GetById(int id)
        {
            var albums = GetAll();
            return albums.FirstOrDefault(a => a.Id == id);
        } 
    }
}
