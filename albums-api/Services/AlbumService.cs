using albums_api.Contracts;
using albums_api.Models;

namespace albums_api.Services;

public sealed class AlbumService : IAlbumService
{
    private readonly object sync = new();
    private readonly List<Album> albums = new()
    {
        new Album(1, "You, Me and an App Id", "Daprize", 2020, 10.99, "https://aka.ms/albums-daprlogo"),
        new Album(2, "Seven Revision Army", "The Blue-Green Stripes", 2021, 13.99, "https://aka.ms/albums-containerappslogo"),
        new Album(3, "Scale It Up", "KEDA Club", 2022, 13.99, "https://aka.ms/albums-kedalogo"),
        new Album(4, "Lost in Translation", "MegaDNS", 2023, 12.99, "https://aka.ms/albums-envoylogo"),
        new Album(5, "Lock Down Your Love", "V is for VNET", 2024, 12.99, "https://aka.ms/albums-vnetlogo"),
        new Album(6, "Sweet Container O' Mine", "Guns N Probeses", 2025, 14.99, "https://aka.ms/albums-containerappslogo")
    };
    private int lastId = 6;

    public IReadOnlyList<AlbumResponse> GetAll()
    {
        lock (sync)
        {
            return albums.Select(ToResponse).ToArray();
        }
    }

    public AlbumResponse? GetById(int id)
    {
        lock (sync)
        {
            var album = albums.FirstOrDefault(a => a.Id == id);
            return album is null ? null : ToResponse(album);
        }
    }

    public IReadOnlyList<AlbumResponse> SearchByYear(int year)
    {
        lock (sync)
        {
            return albums.Where(a => a.Year == year).Select(ToResponse).ToArray();
        }
    }

    public AlbumResponse Create(CreateAlbumRequest request)
    {
        lock (sync)
        {
            int id = checked(lastId + 1);
            var album = new Album(id, request.Title, request.Artist, request.Year, request.Price, request.ImageUrl);
            albums.Add(album);
            lastId = id;
            return ToResponse(album);
        }
    }

    public AlbumResponse? Update(int id, UpdateAlbumRequest request)
    {
        lock (sync)
        {
            int index = albums.FindIndex(a => a.Id == id);
            if (index < 0)
            {
                return null;
            }

            var album = new Album(id, request.Title, request.Artist, request.Year, request.Price, request.ImageUrl);
            albums[index] = album;
            return ToResponse(album);
        }
    }

    public bool Delete(int id)
    {
        lock (sync)
        {
            return albums.RemoveAll(a => a.Id == id) != 0;
        }
    }

    private static AlbumResponse ToResponse(Album album) =>
        new(album.Id, album.Title, album.Artist, album.Year, album.Price, album.ImageUrl);
}
