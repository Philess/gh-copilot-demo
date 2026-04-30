using albums_api.Models;
using Microsoft.AspNetCore.Mvc;

namespace albums_api.Controllers
{
    [Route("albums")]
    [ApiController]
    public class AlbumController : ControllerBase
    {
        // GET: /albums
        [HttpGet]
        public IActionResult Get()
        {
            return Ok(Album.GetAll());
        }

        // GET /albums/{id}
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            var album = Album.GetById(id);
            if (album is null) return NotFound();
            return Ok(album);
        }

        // GET /albums/sorted?sortBy=title|artist|price
        [HttpGet("sorted")]
        public IActionResult GetSorted([FromQuery] string sortBy = "title")
        {
            var albums = Album.GetAll();

            IEnumerable<Album> sorted = sortBy.ToLowerInvariant() switch
            {
                "title"  => albums.OrderBy(a => a.Title),
                "artist" => albums.OrderBy(a => a.Artist.Name),
                "price"  => albums.OrderBy(a => a.Price),
                _ => null!
            };

            if (sorted is null)
                return BadRequest($"Invalid sortBy value '{sortBy}'. Allowed values: title, artist, price.");

            return Ok(sorted);
        }

        // GET /albums/search?year=2020
        [HttpGet("search")]
        public IActionResult Search([FromQuery] int year)
        {
            return Ok(Album.GetByYear(year));
        }

        // POST /albums
        [HttpPost]
        public IActionResult Create([FromBody] Album album)
        {
            var created = Album.Add(album);
            return CreatedAtAction(nameof(Get), new { id = created.Id }, created);
        }

        // PUT /albums/{id}
        [HttpPut("{id}")]
        public IActionResult Update(int id, [FromBody] Album album)
        {
            if (id != album.Id) return BadRequest("ID in URL does not match ID in body.");
            if (!Album.Update(album)) return NotFound();
            return Ok(album);
        }

        // DELETE /albums/{id}
        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            if (!Album.Delete(id)) return NotFound();
            return NoContent();
        }
    }
}
