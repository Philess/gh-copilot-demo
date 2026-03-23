using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860

namespace albums_api.Controllers
{
    [Route("albums")]
    [ApiController]
    public class AlbumController : ControllerBase
    {
        // GET: api/album
        [HttpGet]
        public IActionResult Get()
        {
            var albums = Album.GetAll();

            return Ok(albums);
        }

        // GET api/<AlbumController>/5
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            // here we would normally get the album from a database, but for this example we'll just return a dummy album
            var album = Album.GetById(id);
            if (album == null)
            {
                return NotFound();
            }

            return Ok(album);

        }

        [HttpGet("year/{year:int}")]
        public IActionResult GetByYear(int year)
        {
            var albums = Album.GetByYear(year);
            return Ok(albums);
        }

        // function that retrieves albums and sorts them by title, artist or price
        [HttpGet("sorted")]
        public IActionResult GetSorted(string sortBy)
        {
            if (string.IsNullOrWhiteSpace(sortBy))
            {
                return BadRequest("A sort option is required.");
            }

            var albums = Album.GetAll();

            switch (sortBy.ToLower())
            {
                case "title":
                    albums = albums.OrderBy(a => a.Title).ToList();
                    break;
                case "artist":
                    albums = albums.OrderBy(a => a.Artist.Name).ToList();
                    break;
                case "price":
                    albums = albums.OrderBy(a => a.Price).ToList();
                    break;
                default:
                    return BadRequest("Invalid sort option");
            }

            return Ok(albums);
        }

        [HttpPost]
        public IActionResult Create([FromBody] AlbumRequest request)
        {
            var album = Album.Create(request);
            return CreatedAtAction(nameof(Get), new { id = album.Id }, album);
        }

        [HttpPut("{id:int}")]
        public IActionResult Update(int id, [FromBody] AlbumRequest request)
        {
            var updatedAlbum = Album.Update(id, request);
            if (updatedAlbum == null)
            {
                return NotFound();
            }

            return Ok(updatedAlbum);
        }

        [HttpDelete("{id:int}")]
        public IActionResult Delete(int id)
        {
            var deleted = Album.Delete(id);
            if (!deleted)
            {
                return NotFound();
            }

            return NoContent();
        }
    }
}
