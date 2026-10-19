namespace albums_api.Models
{
    public sealed record Album(int Id, string Title, string Artist, int Year, double Price, string ImageUrl);
}
