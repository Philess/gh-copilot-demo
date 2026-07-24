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
            return Ok();
        }

        // GET api/albums/sort?by=title|artist|price
        [HttpGet("sort")]
        public IActionResult GetSorted([FromQuery] string by = "title")
        {
            var albums = Album.GetAll();

            var sortedAlbums = by.ToLower() switch
            {
                "artist" => albums.OrderBy(a => a.Artist).ToList(),
                "price" => albums.OrderBy(a => a.Price).ToList(),
                "title" or _ => albums.OrderBy(a => a.Title).ToList(),
            };

            return Ok(sortedAlbums);
        }

    }
}

