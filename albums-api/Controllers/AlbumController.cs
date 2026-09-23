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
    /// <summary>
    /// Controller for managing albums.
    /// </summary>
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

        // function that retrieves albums and sorts them by title, artist or price
        [HttpGet("sorted")]
        public IActionResult GetSorted(string sortBy)
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
                    return BadRequest("Invalid sort parameter. Use 'title', 'artist', or 'price'.");
            }

            return Ok(albums);
        }

        // function that retrieves albums released in a given year
        [HttpGet("search")]
        public IActionResult Search(int year)
        {
            var albums = Album.GetByYear(year);

            return Ok(albums);
        }

        // POST api/<AlbumController>
        [HttpPost]
        public IActionResult Create([FromBody] Album album)
        {
            if (album is null)
            {
                return BadRequest("Album data is required.");
            }

            var createdAlbum = Album.Create(album);

            return CreatedAtAction(nameof(Get), new { id = createdAlbum.Id }, createdAlbum);
        }

        // PUT api/<AlbumController>/5
        [HttpPut("{id}")]
        public IActionResult Update(int id, [FromBody] Album album)
        {
            if (album is null)
            {
                return BadRequest("Album data is required.");
            }

            var updatedAlbum = Album.Update(id, album);

            if (updatedAlbum is null)
            {
                return NotFound();
            }

            return Ok(updatedAlbum);
        }

        // DELETE api/<AlbumController>/5
        [HttpDelete("{id}")]
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
