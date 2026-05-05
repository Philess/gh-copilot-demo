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
            var album = Album.GetById(id);

            if (album is null)
            {
                return NotFound();
            }

            return Ok(album);
        }

        [HttpGet("search")]
        public IActionResult SearchByYear([FromQuery] int year)
        {
            var albums = Album.GetByYear(year);
            return Ok(albums);
        }

        [HttpPost]
        public IActionResult Post([FromBody] Album album)
        {
            var createdAlbum = Album.Create(album);
            return CreatedAtAction(nameof(Get), new { id = createdAlbum.Id }, createdAlbum);
        }

        [HttpPut("{id}")]
        public IActionResult Put(int id, [FromBody] Album album)
        {
            var updatedAlbum = Album.Update(id, album);

            if (updatedAlbum is null)
            {
                return NotFound();
            }

            return Ok(updatedAlbum);
        }

        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            var wasDeleted = Album.Delete(id);

            if (!wasDeleted)
            {
                return NotFound();
            }

            return NoContent();
        }

    }
}
