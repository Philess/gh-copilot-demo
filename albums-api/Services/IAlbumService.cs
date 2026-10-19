using albums_api.Contracts;

namespace albums_api.Services;

public interface IAlbumService
{
    IReadOnlyList<AlbumResponse> GetAll();
    AlbumResponse? GetById(int id);
    IReadOnlyList<AlbumResponse> SearchByYear(int year);
    AlbumResponse Create(CreateAlbumRequest request);
    AlbumResponse? Update(int id, UpdateAlbumRequest request);
    bool Delete(int id);
}
