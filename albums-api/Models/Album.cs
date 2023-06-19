namespace albums_api.Models
{
    public record Album(int Id, string Title, string Artist, double Price, string Image_url, string Genre)
    {
        public static List<Album> GetAll()
        {
            // generate a string table of 6 random music genre in a genres variable
            var genres = new List<string>() { "Rock", "Pop", "Jazz", "Metal", "Rap", "Country" }; 

            // replace the "rock" genre in the albums variable with the random genres from the genres variable
            var albums = new List<Album>(){
            new Album(1, "You, Me and an App Id", "Daprize", 10.99, "https://aka.ms/albums-daprlogo", genres[0]),
            new Album(2, "Seven Revision Army", "The Blue-Green Stripes", 13.99, "https://aka.ms/albums-containerappslogo", genres[1]),
            new Album(3, "Scale It Up", "KEDA Club", 13.99, "https://aka.ms/albums-kedalogo", genres[2]),
            new Album(4, "Lost in Translation", "MegaDNS", 12.99,"https://aka.ms/albums-envoylogo", genres[3]),
            new Album(5, "Lock Down Your Love", "V is for VNET", 12.99, "https://aka.ms/albums-vnetlogo", genres[4]),
            new Album(6, "Sweet Container O' Mine", "Guns N Probeses", 14.99, "https://aka.ms/albums-containerappslogo", genres[5]),
         };
         
            return albums;
        }
    }
}
