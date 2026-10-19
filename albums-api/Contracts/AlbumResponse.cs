using System.Text.Json.Serialization;

namespace albums_api.Contracts;

/// <summary>An album returned by the API, including its server-assigned ID.</summary>
public sealed record AlbumResponse(
    int Id,
    string Title,
    string Artist,
    int Year,
    double Price,
    [property: JsonPropertyName("image_url")] string ImageUrl);
