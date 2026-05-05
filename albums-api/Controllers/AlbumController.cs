using albums_api.Models;
using Microsoft.AspNetCore.Mvc;
using System.Net;
using System.Text.Json;
using System.Text;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860

namespace albums_api.Controllers
{
    public record CreateAlbumRequest(string Title, Artist Artist, int Year, double Price, string Image_url);
    public record UpdateAlbumRequest(string Title, Artist Artist, int Year, double Price, string Image_url);

    /// <summary>
    /// Controller for managing albums. Provides endpoints to retrieve all albums, retrieve a specific album by ID, sort albums by title, artist, or price, search by year, and create, update or delete albums.
    /// </summary>
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
            //here we will retrieve the album with the specified id from the database
            var album = Album.GetById(id);
            if (album == null)
            {
                return NotFound();
            }
            return Ok(album);
        }

        // function that retrieves albums and sorts them by title, artist or price
        [HttpGet("sort")]
        public IActionResult Get(string? sortBy)
        {
            if (string.IsNullOrWhiteSpace(sortBy))
            {
                return BadRequest("sortBy parameter is required. Valid values are: title, artist, price.");
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
                    return BadRequest("Invalid sortBy parameter. Valid values are: title, artist, price.");
            }
            return Ok(albums);
        }

        // GET /albums/search?year=2021
        [HttpGet("search")]
        public IActionResult Search([FromQuery] int year)
        {
            var albums = Album.GetByYear(year);
            return Ok(albums);
        }

        // POST /albums
        [HttpPost]
        public IActionResult Post([FromBody] CreateAlbumRequest request)
        {
            var album = Album.Create(request.Title, request.Artist, request.Year, request.Price, request.Image_url);
            return CreatedAtAction(nameof(Get), new { id = album.Id }, album);
        }

        // PUT /albums/{id}
        [HttpPut("{id}")]
        public IActionResult Put(int id, [FromBody] UpdateAlbumRequest request)
        {
            var album = Album.Update(id, request.Title, request.Artist, request.Year, request.Price, request.Image_url);
            if (album == null)
            {
                return NotFound();
            }
            return Ok(album);
        }

        // DELETE /albums/{id}
        [HttpDelete("{id}")]
        public IActionResult Delete(int id)
        {
            if (!Album.Delete(id))
            {
                return NotFound();
            }
            return NoContent();
        }
    }
}
