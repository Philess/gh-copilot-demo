using System.ComponentModel.DataAnnotations;
using System.Text.Json.Serialization;

namespace albums_api.Contracts;

/// <summary>All fields required to create an album; its ID is assigned by the server.</summary>
public sealed record CreateAlbumRequest
{
    [Required]
    public required string Title { get; init; }

    [Required]
    public required string Artist { get; init; }

    [Required, Range(1, 9999)]
    public required int Year { get; init; }

    [Required, Range(0, double.MaxValue)]
    public required double Price { get; init; }

    [Required, HttpImageUrl, JsonPropertyName("image_url")]
    public required string ImageUrl { get; init; }
}
