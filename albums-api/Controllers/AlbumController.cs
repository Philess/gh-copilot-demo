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
        // GET: api/album
        [HttpGet]
        public IActionResult Get()
        {
            var albums = Album.GetAll();
            return Ok(albums);
        }

        // GET api/albums/{id}
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            var album = Album.Get().FirstOrDefault(a => a.Id == id);
            if (album == null)
                return NotFound();
            return Ok(album);
        }

        // POST api/albums
        [HttpPost]
        public IActionResult Create([FromBody] Album album)
        {
            if (album == null)
                return BadRequest();

            Album.Add(album);
            return CreatedAtAction(nameof(Get), new { id = album.Id }, album);
        }

        // PUT api/albums/{id}
        [HttpPut("{id}")]
        public IActionResult Update(int id, [FromBody] Album updatedAlbum)
        {
            var album = Album.Get().FirstOrDefault(a => a.Id == id);
            if (album == null)
                return NotFound();

            Album.Update(id, updatedAlbum);
            return NoContent();
        }

        // DELETE api/albums/{id}
        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            var album = Album.Get().FirstOrDefault(a => a.Id == id);
            if (album == null)
                return NotFound();

            Album.Delete(id);
            return NoContent();
        }

        // GET api/albums/search?year=YYYY
        [HttpGet("search")]
        public IActionResult SearchByYear([FromQuery] int year)
        {
            var albums = Album.Get().Where(a => a.Year == year).ToList();
            return Ok(albums);
        }
    }
}
