using System.ComponentModel.DataAnnotations;

namespace albums_api.Contracts;

internal sealed class HttpImageUrlAttribute : ValidationAttribute
{
    public HttpImageUrlAttribute() : base("The image URL must be an absolute HTTP or HTTPS URL.")
    {
    }

    public override bool IsValid(object? value)
    {
        return value is string url &&
            Uri.TryCreate(url, UriKind.Absolute, out var uri) &&
            (uri.Scheme == Uri.UriSchemeHttp || uri.Scheme == Uri.UriSchemeHttps);
    }
}
