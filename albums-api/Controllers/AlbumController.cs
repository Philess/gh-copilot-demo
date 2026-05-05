using albums_api.Models;
using Microsoft.AspNetCore.Mvc;
using System.Net;
using System.Text.Json;
using System.Text;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860

namespace albums_api.Controllers
{
    [Route("albums")]
    [ApiController]
    public class AlbumController : ControllerBase
    {
        // GET: albums
        [HttpGet]
        public IActionResult Get()
        {
            var albums = Album.GetAll();
            return Ok(albums);
        }

        // GET: albums/{id}
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            var album = Album.GetById(id);
            if (album == null)
            {
                return NotFound();
            }
            return Ok(album);
        }

        // POST: albums
        [HttpPost]
        public IActionResult Create([FromBody] Album album)
        {
            if (album == null)
                return BadRequest();
            var created = Album.Create(album);
            return CreatedAtAction(nameof(Get), new { id = created.Id }, created);
        }

        // PUT: albums/{id}
        [HttpPut("{id}")]
        public IActionResult Update(int id, [FromBody] Album album)
        {
            if (album == null)
                return BadRequest();
            var exists = Album.Update(id, album);
            if (!exists)
                return NotFound();
            return NoContent();
        }

        // DELETE: albums/{id}
        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            var deleted = Album.Delete(id);
            if (!deleted)
                return NotFound();
            return NoContent();
        }

        // GET: albums/search?year=2022
        [HttpGet("search")]
        public IActionResult SearchByYear([FromQuery] int year)
        {
            var results = Album.SearchByYear(year);
            return Ok(results);
        }

    }
}
