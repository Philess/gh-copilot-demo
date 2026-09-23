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
            //here
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
                    albums = albums.OrderBy(a => a.Artist).ToList();
                    break;
                case "price":
                    albums = albums.OrderBy(a => a.Price).ToList();
                    break;
                default:
                    return BadRequest("Invalid sort parameter. Use 'title', 'artist', or 'price'.");
            }

            return Ok(albums);
        }               

    }
}
