using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860
/// <summary>
/// AlbumController is responsible for handling HTTP requests related to albums.
/// </summary>
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
            //here we are using the GetById method to get a specific album by its id
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
            return Ok(Album.GetByYear(year));
        }

        [HttpPost]
        public IActionResult Create([FromBody] Album album)
        {
            var createdAlbum = Album.Create(album);
            return CreatedAtAction(nameof(Get), new { id = createdAlbum.Id }, createdAlbum);
        }

        [HttpPut("{id:int}")]
        public IActionResult Update(int id, [FromBody] Album album)
        {
            var updatedAlbum = Album.Update(id, album);
            return updatedAlbum == null ? NotFound() : Ok(updatedAlbum);
        }

        [HttpDelete("{id:int}")]
        public IActionResult Delete(int id)
        {
            return Album.Delete(id) ? NoContent() : NotFound();
        }

        // function that retrieves albums and sorts them by title, artist or price
        [HttpGet("sort")]
        public IActionResult Sort(string sortBy)
        {
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
                    return BadRequest("Invalid sort parameter. Please use 'title', 'artist', or 'price'.");
            }

            return Ok(albums);
        }

    }
}
