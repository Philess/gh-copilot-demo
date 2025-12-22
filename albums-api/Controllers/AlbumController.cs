using albums_api.Models;
using Microsoft.AspNetCore.Mvc;
using System.Net;
using System.Text.Json;
using System.Text;
using System.Linq;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860

namespace albums_api.Controllers
{
    [Route("albums")]
    [ApiController]
    public class AlbumController : ControllerBase
    {
        // GET: api/album
        [HttpGet]
        public IActionResult Get([FromQuery] string? sort = null)
        {
            var albums = Album.GetAll();

            if (!string.IsNullOrWhiteSpace(sort))
            {
                switch (sort.Trim().ToLowerInvariant())
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
                        break;
                }
            }

            return Ok(albums);
        }

        // GET api/<AlbumController>/5
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            return Ok();
        }

    }
}
