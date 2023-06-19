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
        /// <returns>A list of albums</returns>
        /// <response code="200">Returns the list of albums</response>
        /// <response code="404">If no albums are found</response>
        /// <response code="500">If there was an internal server error</response>
        /// <remarks>
        /// Sample request: GET api/albums
        /// </remarks>
        [HttpGet]
        public IActionResult Get()
        {
            var albums = Album.GetAll();

            return Ok(albums);
        }

        /// <summary>
        /// Get an album by id
        /// </summary>
        /// <param name="id">The id of the album</param>
        /// <returns>An album</returns>
        /// <response code="200">Returns the album</response>
        /// <response code="404">If the album is not found</response>
        /// <response code="500">If there was an internal server error</response>
        /// <remarks>
        /// Sample request: GET api/albums/1
        /// </remarks>
        [HttpGet("{id}")]
        public IActionResult Get(int id)
        {
            // Get all the albums from the Album class and extract the one with the matching id
            var album = Album.GetAll().Find(album => album.Id == id);
            
            // If the album is null, return a 404
            if (album == null)
            {
                return NotFound();
            }

            // Otherwise, return the album
            return Ok(album);
        }

        /// <summary>
        /// function that search album by title, artist or genre
        /// </summary>
        /// <param name="q">The title, artist or genre of the album</param>
        /// <returns>An album</returns>
        /// <response code="200">Returns the album</response>
        /// <response code="404">If the album is not found</response>
        /// <response code="500">If there was an internal server error</response>
        /// <remarks>
        /// Sample request: GET api/albums/search?q=rock
        /// </remarks>
        [HttpGet("search")]
        public IActionResult Search([FromQuery] string q)
        {
            // Get all the albums from the Album class and extract the one with the matching id
            var albums = Album.GetAll().FindAll(album => album.Title.Contains(q) || album.Artist.Contains(q) || album.Genre.Contains(q));
            
            // If the album is null, return a 404
            if (albums == null)
            {
                return NotFound();
            }

            // Otherwise, return the album
            return Ok(albums);
        }

        /// <summary>
        /// function that sorts album by title, artist or genre
        /// </summary>
        /// <param name="sort">The album field to look for : title, artist or genre</param>
        /// <returns>An album</returns>
        /// <response code="200">Returns the album</response>
        /// <response code="404">If the album is not found</response>
        /// <response code="500">If there was an internal server error</response>
        /// <remarks>
        /// Sample request: GET api/albums/sort?sort=genre
        /// </remarks>
        [HttpGet("sort")]
        public IActionResult Sort([FromQuery] string sort)
        {
            // Get all the albums from the Album class and extract the one with the matching id
            var albums = Album.GetAll();
            
            // If the album is null, return a 404
            if (albums == null)
            {
                return NotFound();
            }

            // Otherwise, return the album
            return sort switch
            {
                "name" => Ok(albums.OrderBy(album => album.Title)),
                "artist" => Ok(albums.OrderBy(album => album.Artist)),
                "genre" => Ok(albums.OrderBy(album => album.Genre)), 
                //otherwise, return an error message stating it needs to be set to name, artist, or genre
                _ => BadRequest("sort must be set to name, artist, or genre")
            };
        }
    }
}
