namespace albums_api.Models
{
    /// <summary>
    /// Represents a music artist.
    /// </summary>
    public record Artist(int Id, string Name, string Genre)
    {
        /// <summary>
        /// Returns a static list of artists.
        /// </summary>
        public static List<Artist> GetAll()
        {
            var artists = new List<Artist>()
            {
                new Artist(1, "Daprize", "Pop"),
                new Artist(2, "The Blue-Green Stripes", "Rock"),
                new Artist(3, "KEDA Club", "Electronic"),
                new Artist(4, "MegaDNS", "Alternative"),
                new Artist(5, "V is for VNET", "Indie"),
                new Artist(6, "Guns N Probeses", "Rock")
            };
            return artists;
        }
    }
}
