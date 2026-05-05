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
        public IActionResult Get(string sortBy)
        {            
            var albums = Album.GetAll();
            switch (sortBy.ToLower())
            {                
                case "title":
                    albums = albums.OrderBy(a => a.Title).ToList();
                    break;
                case "artist":
                    albums = albums.OrderBy(a => a.Artist).ToList();
                    break;
                case "price":
                    albums = albums.OrderBy(a => a.Price).ToList();
                    break;
                default:                
                    return BadRequest("Invalid sortBy parameter. Valid values are: title, artist, price.");
            }
            return Ok(albums);
        }
    }
}
