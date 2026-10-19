using albums_api.Contracts;
using albums_api.Services;
using Microsoft.AspNetCore.Mvc;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;

namespace albums_api.Controllers
{
    /// <summary>
    /// Controller for managing albums.
    /// Provides endpoints to retrieve, sort, search, create, update, and delete albums.
    /// </summary>
    [Route("albums")]
    [ApiController]
    public class AlbumController : ControllerBase
    {
        private readonly IAlbumService albumService;

        public AlbumController(IAlbumService albumService)
        {
            this.albumService = albumService;
        }

        /// <summary>Returns all albums in the current application instance.</summary>
        [HttpGet]
        [ProducesResponseType(typeof(IReadOnlyList<AlbumResponse>), StatusCodes.Status200OK)]
        public ActionResult<IReadOnlyList<AlbumResponse>> Get(CancellationToken cancellationToken)
        {
            return Ok(albumService.GetAll());
        }

        /// <summary>Returns an album by its ID.</summary>
        [HttpGet("{id}")]
        [ProducesResponseType(typeof(AlbumResponse), StatusCodes.Status200OK)]
        [ProducesResponseType(typeof(ProblemDetails), StatusCodes.Status404NotFound)]
        public ActionResult<AlbumResponse> GetById(int id, CancellationToken cancellationToken)
        {
            var album = albumService.GetById(id);
            if (album == null)
            {
                return NotFound();
            }
            return Ok(album);
        }

        /// <summary>Returns albums sorted by title, artist, or price.</summary>
        [HttpGet("sort")]
        [ProducesResponseType(typeof(IReadOnlyList<AlbumResponse>), StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public ActionResult<IReadOnlyList<AlbumResponse>> GetSorted(
            [FromQuery] string sortBy, CancellationToken cancellationToken)
        {
            IEnumerable<AlbumResponse> albums = albumService.GetAll();
            switch (sortBy.ToLower())
            {
                case "title":
                    albums = albums.OrderBy(a => a.Title).ToList();
                    break;
                case "artist":
                    albums = albums.OrderBy(a => a.Artist).ToList();
                    break;
                case "price":
                    albums = albums.OrderBy(a => a.Price).ToList();
                    break;
                default:
                    return BadRequest("Invalid sort parameter");
            }
            return Ok(albums);
        }

        /// <summary>Returns albums released in exactly the specified year.</summary>
        [HttpGet("search")]
        [ProducesResponseType(typeof(IReadOnlyList<AlbumResponse>), StatusCodes.Status200OK)]
        [ProducesResponseType(typeof(ValidationProblemDetails), StatusCodes.Status400BadRequest)]
        public ActionResult<IReadOnlyList<AlbumResponse>> Search(
            [FromQuery, BindRequired, Range(1, 9999)] int year, CancellationToken cancellationToken)
        {
            return Ok(albumService.SearchByYear(year));
        }

        /// <summary>Creates an album with a server-assigned ID.</summary>
        [HttpPost]
        [ProducesResponseType(typeof(AlbumResponse), StatusCodes.Status201Created)]
        [ProducesResponseType(typeof(ValidationProblemDetails), StatusCodes.Status400BadRequest)]
        public ActionResult<AlbumResponse> Create(
            [FromBody] CreateAlbumRequest request, CancellationToken cancellationToken)
        {
            var album = albumService.Create(request);
            return CreatedAtAction(nameof(GetById), new { id = album.Id }, album);
        }

        /// <summary>Replaces all fields of an existing album, preserving its ID.</summary>
        [HttpPut("{id}")]
        [ProducesResponseType(typeof(AlbumResponse), StatusCodes.Status200OK)]
        [ProducesResponseType(typeof(ValidationProblemDetails), StatusCodes.Status400BadRequest)]
        [ProducesResponseType(typeof(ProblemDetails), StatusCodes.Status404NotFound)]
        public ActionResult<AlbumResponse> Update(
            int id, [FromBody] UpdateAlbumRequest request, CancellationToken cancellationToken)
        {
            var album = albumService.Update(id, request);
            return album is null ? NotFound() : Ok(album);
        }

        /// <summary>Deletes an existing album.</summary>
        [HttpDelete("{id}")]
        [ProducesResponseType(StatusCodes.Status204NoContent)]
        [ProducesResponseType(typeof(ProblemDetails), StatusCodes.Status404NotFound)]
        public IActionResult Delete(int id, CancellationToken cancellationToken)
        {
            return albumService.Delete(id) ? NoContent() : NotFound();
        }
    }
}
