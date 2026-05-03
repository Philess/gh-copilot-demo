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
        // GET: /albums
        [HttpGet]
        public IActionResult Get()
        {
            var albums = Album.GetAll();
            return Ok(albums);
        }

        // GET: /albums/{id}
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            var album = Album.GetById(id);
            
            if (album is null)
            {
                return NotFound(new { message = $"Album with ID {id} not found" });
            }

            return Ok(album);
        }

        // GET: /albums/search?year=2021
        [HttpGet("search")]
        public IActionResult SearchByYear([FromQuery] int year)
        {
            var albums = Album.GetByYear(year);
            return Ok(albums);
        }

        // POST: /albums
        [HttpPost]
        public IActionResult Create([FromBody] CreateAlbumRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var artist = new Artist(
                request.ArtistName,
                request.ArtistBirthdate,
                request.ArtistBirthPlace
            );

            var album = Album.Create(
                request.Title,
                artist,
                request.Price,
                request.Year,
                request.Image_url
            );

            return CreatedAtAction(nameof(Get), new { id = album.Id }, album);
        }

        // PUT: /albums/{id}
        [HttpPut("{id}")]
        public IActionResult Update(int id, [FromBody] UpdateAlbumRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var artist = new Artist(
                request.ArtistName,
                request.ArtistBirthdate,
                request.ArtistBirthPlace
            );

            var success = Album.Update(
                id,
                request.Title,
                artist,
                request.Price,
                request.Year,
                request.Image_url
            );

            if (!success)
            {
                return NotFound(new { message = $"Album with ID {id} not found" });
            }

            var updatedAlbum = Album.GetById(id);
            return Ok(updatedAlbum);
        }

        // DELETE: /albums/{id}
        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            var success = Album.Delete(id);

            if (!success)
            {
                return NotFound(new { message = $"Album with ID {id} not found" });
            }

            return NoContent();
        }
    }

    /// <summary>
    /// Request model for creating a new album
    /// </summary>
    public class CreateAlbumRequest
    {
        public string Title { get; set; } = string.Empty;
        public string ArtistName { get; set; } = string.Empty;
        public DateTime? ArtistBirthdate { get; set; }
        public string ArtistBirthPlace { get; set; } = string.Empty;
        public double Price { get; set; }
        public int Year { get; set; }
        public string Image_url { get; set; } = string.Empty;
    }

    /// <summary>
    /// Request model for updating an existing album
    /// </summary>
    public class UpdateAlbumRequest
    {
        public string Title { get; set; } = string.Empty;
        public string ArtistName { get; set; } = string.Empty;
        public DateTime? ArtistBirthdate { get; set; }
        public string ArtistBirthPlace { get; set; } = string.Empty;
        public double Price { get; set; }
        public int Year { get; set; }
        public string Image_url { get; set; } = string.Empty;
    }
}
