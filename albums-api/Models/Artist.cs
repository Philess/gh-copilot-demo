namespace albums_api.Models
{
    /// <summary>
    /// Represents an artist with biographical information
    /// </summary>
    public record Artist(string Name, DateTime? Birthdate, string BirthPlace);
}
