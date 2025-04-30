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
        
        
        /// <summary>
        /// Get all albums
        /// </summary>
        /// <returns></returns>
        [HttpGet]
        public IActionResult Get()
        {
            var albums = Album.GetAll();

            return Ok(albums);
        }

        /// <summary>
        /// Get album by id
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
        /// <summary>
        /// Create a new album
        /// </summary>
        /// <param name="album">The album to create</param>
        /// <returns>A newly created album with its ID</returns>
        [HttpPost]
        public IActionResult Create([FromBody] Album album)
        {
            if (album == null)
            {
                return BadRequest("Album is required.");
            }

            Album.Create(album);
            return CreatedAtAction(nameof(Get), new { id = album.Id }, album);
        }

    }
}
