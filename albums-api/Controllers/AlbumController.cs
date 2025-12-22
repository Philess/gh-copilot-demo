using albums_api.Models;
using Microsoft.AspNetCore.Mvc;
using System.Net;
using System.Text.Json;
using System.Text;
using System.Linq;

namespace albums_api.Controllers
{
    /// <summary>
    /// Album API Controller
    /// </summary>
    [Route("albums")]
    [ApiController]
    public class AlbumController : ControllerBase
    {
        // GET: api/albums
        [HttpGet]
        public IActionResult Get([FromQuery] string? sort = null, [FromQuery] int? year = null)
        {
            var albums = Album.GetAll();

            if (year.HasValue)
            {
                albums = albums.Where(a => a.Year == year.Value).ToList();
            }

            if (!string.IsNullOrWhiteSpace(sort))
            {
                switch (sort.Trim().ToLowerInvariant())
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
                        break;
                }
            }

            return Ok(albums);
        }

        /// <summary>
        /// Get album by ID
        /// </summary>
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

        /// <summary>
        /// Create a new album
        /// </summary>
        [HttpPost]
        public IActionResult Post([FromBody] Album album)
        {
            if (album == null)
            {
                return BadRequest();
            }
            // In a real application, you would save to a database
            return CreatedAtAction(nameof(Get), new { id = album.Id }, album);
        }

        /// <summary>
        /// Update an existing album
        /// </summary>
        [HttpPut("{id}")]
        public IActionResult Put(int id, [FromBody] Album album)
        {
            var existingAlbum = Album.GetById(id);
            if (existingAlbum == null)
            {
                return NotFound();
            }
            // In a real application, you would update the database
            return Ok(album);
        }

        /// <summary>
        /// Delete an album by ID
        /// </summary>
        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            var album = Album.GetById(id);
            if (album == null)
            {
                return NotFound();
            }
            // In a real application, you would delete from the database
            return NoContent();
        }
    }
}
